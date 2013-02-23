/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import scala.reflect.internal.ClassfileConstants._
import scala.collection.{ mutable, immutable }
// import symtab._
// import Flags._
// import scala.reflect.internal.Mode._

trait Signatures extends scala.reflect.internal.transform.Erasure {
  val global: Global

  import global._
  import definitions._

  // @M don't generate java generics sigs for (members of) implementation
  // classes, as they are monomorphic (TODO: ok?)
  private def requiresSignature(sym: Symbol) = !(
    // PP: This condition used to include sym.hasExpandedName, but this leads
    // to the total loss of generic information if a private member is
    // accessed from a closure: both the field and the accessor were generated
    // without it.  This is particularly bad because the availability of
    // generic information could disappear as a consequence of a seemingly
    // unrelated change.
       settings.Ynogenericsig.value
    || sym.isArtifact
    || sym.isLiftedMethod
    || sym.isBridge
    || sym.ownerChain.exists(_.isImplClass)
  )

  /** @return
   *   - `null` if no Java signature is to be added (`null` is what ASM expects in these cases).
   *   - otherwise the signature in question
   */
  def jvmSignature(member: Symbol, owner: Symbol): Option[String] = {
    if (!requiresSignature(member)) None
    else Some(new SignatureMap(member, owner) signature) filter { sig =>
      log(sig)
      !settings.Xverify.value || verifySignature(member, sig)
    }
  }

  // class MethodSignatureCalculator(val member: MethodSymbol, val owner: Symbol) {
  //   val prefix     = owner.thisType
  //   val unerased   = prefix memberInfo member
  //   val matching   = member matchingSymbol owner.thisType
  //   val root       = if (matching.ownerChain contains owner) matching else owner
  //   val enclosures = root.ownerChain takeWhile (s => !s.isPackageClass)
  //   val visible    =
  //     enclosures.flatMap(_.typeParams).foldLeft(Map[Name, Symbol]())((res, tparam) =>
  //       if (res contains tparam.name) res
  //       else res + (tparam.name -> tparam)
  //     )
  // }

  class SignatureMap(val member: Symbol, val owner: Symbol) {
    private val prefix            = owner.thisType
    private val unerased          = enteringErasure(prefix memberInfo member)
    private val matching          = member matchingSymbol owner.thisType
    private val root              = if (matching.ownerChain contains owner) matching else owner
    private var inBoxedPosition   = false
    private val isGeneric         = member.info.typeParams.nonEmpty
    private val isTraitSignature  = member.isTrait
    private val boundExistentials = mutable.Set[Symbol]()

    lazy val signature = jsig(unerased)

    def isBound(tp: Type) = boundExistentials(tp.typeSymbol)
    def withExistentials(tparams: List[Symbol])(underlying: Type): String = {
      boundExistentials ++= tparams
      try jsig(underlying)
      finally boundExistentials --= tparams
    }
    def methodSig(params: List[Symbol], restpe: Type): String = {
      val parameterSig = params map (p => jsig(p.tpe_*)) mkString ("(", "", ")")
      val resultSig = if (restpe.typeSymbol == UnitClass || member.isConstructor) VOID_TAG else jsig(restpe)

      parameterSig + resultSig
    }
    def tparamsSig(tparams: List[Symbol]): String = (
      if (tparams.isEmpty) ""
      else tparams map tparamSig mkString ("<", "", ">")
    )
    def tparamSig(tparam: Symbol): String =
      "" + tparam.name + boundsSig(tparam.info.bounds)

    def boundsSig(bounds: TypeBounds): String = {
      val parents = bounds.hi.dealiasWiden match {
        case RefinedType(parents, _) => parents
        case tp                      => tp :: Nil
      }
      val (isClass, rest) = parents partition (s => s.typeSymbol.isClass && !s.typeSymbol.isTrait)
      val classPart = isClass match {
        case Nil    => ":"
        case x :: _ => ":" + jsig.boxed(x)
      }
      classPart :: (rest map jsig.boxed) mkString ":"
    }

    def polySig(tparams: List[Symbol], restpe: Type): String = tparamsSig(tparams) + jsig(restpe)

    def superSig(parents: List[Type]): String = {
      def normalize(parents: List[Type]): List[Type] = {
        // java is unthrilled about seeing interfaces inherit from classes
        def ok(p: Type) = p.typeSymbol.isTrait || p.typeSymbol.isInterface
        if (isTraitSignature)
          ObjectClass.tpe :: (parents filter ok) distinct // traits always list Object
        else
          parents
      }
      normalize(parents) map jsig.boxed mkString ""
    }
    // Anything which could conceivably be a module (i.e. isn't known to be
    // a type parameter or similar) must go through here or the signature is
    // likely to end up with Foo<T>.Empty where it needs Foo<T>.Empty$.
    def fullNameInSig(sym: Symbol) = "L" + enteringIcode(sym.javaBinaryName)

    def typeRefSig(tp: TypeRef): String = {
      val TypeRef(pre, sym, args) = tp.dealiasWiden
      def args_s = args map jsig mkString ""
      def valueClassSig: String = {
        val unboxed     = sym.derivedValueClassUnbox.info.finalResultType
        val unboxedSeen = (tp memberType sym.derivedValueClassUnbox).finalResultType
        def unboxedMsg  = if (unboxed == unboxedSeen) "" else s", seen within ${sym.simpleName} as $unboxedSeen"

        logResult(s"Erasure of value class $sym (underlying type $unboxed$unboxedMsg) is") {
          if (isPrimitiveValueType(unboxedSeen) && inBoxedPosition) classSig
          else jsig(unboxedSeen)
        }
      }
      def classSig = {
        val preRebound = pre.baseType(member.owner) // #2585
        dotCleanup(
          (
            if (needsJavaSig(preRebound)) {
              val s = jsig(preRebound)
              if (s.charAt(0) == 'L') s.substring(0, s.length - 1) + "." + sym.javaSimpleName
              else fullNameInSig(sym)
            }
            else fullNameInSig(sym)
          ) + (
            if (args.isEmpty) "" else
            "<"+(args map argSig).mkString+">"
          ) + (
            ";"
          )
        )
      }

      // If args isEmpty, Array is being used as a type constructor
      if (isTypeParameterInSig(sym, root))
        "" + TVAR_TAG + sym.name + ";"
      else if (isPrimitiveValueClass(sym)) {
        if (inBoxedPosition) jsig(ObjectClass.tpe)
        else if (sym == UnitClass) jsig(BoxedUnitClass.tpe)
        else "" + abbrvTag(sym)
      }
      else sym match {
        case ArrayClass if args.nonEmpty && unboundedGenericArrayLevel(tp) != 1 => ARRAY_TAG + args_s
        case ArrayClass | AnyClass | AnyValClass | SingletonClass               => jsig(ObjectClass.tpe)
        case UnitClass                                                          => jsig(BoxedUnitClass.tpe)
        case NothingClass                                                       => jsig(RuntimeNothingClass.tpe)
        case NullClass                                                          => jsig(RuntimeNullClass.tpe)
        case _  if sym.isClass                                                  => classSig
        case _                                                                  => jsig(erasure(member)(tp))
      }
    }

    def argSig(tp: Type): String = {
      def bounds     = tp.typeSymbol.info.bounds
      def hasHiBound = !(AnyRefClass.tpe <:< bounds.hi)
      def hasLoBound = !(bounds.lo <:< NullClass.tpe)

      if (!isBound(tp)) jsig.boxed(tp)
      else if (hasHiBound) "+" + jsig.boxed(bounds.hi)
      else if (hasLoBound) "-" + jsig.boxed(bounds.lo)
      else "*"
    }

    private def msg(tp: Type) = enteringErasure {
      s"""|Confused by appearance of $tp while calculating signature of $member
          |      defined in ${member.owner.defString}
          |  calculating in ${owner.defString}
          |  member: ${member.defString}
          |  erased: ${exitingPostErasure(member.defString)}
          | matching: $matching in ${matching.owner}
          |     root: $root in ${root.owner}
          |""".stripMargin
    }
    object jsig extends (Type => String) {
      def boxed(tp: Type): String = {
        val saved = inBoxedPosition
        inBoxedPosition = true
        try jsig(tp) finally inBoxedPosition = saved
      }
      def apply(tp: Type): String = tp.dealiasWiden match {
        case AnnotatedType(_, atp, _)      => apply(atp)
        case ExistentialType(tparams, tpe) => withExistentials(tparams)(tpe)
        case PolyType(tparams, restpe)     => polySig(tparams, restpe)
        case MethodType(params, restpe)    => methodSig(params, restpe)
        case RefinedType(parent :: _, _)   => boxed(parent)
        case ClassInfoType(parents, _, _)  => superSig(parents)
        case tp @ TypeRef(_, _, _)         => typeRefSig(tp)
        case WildcardType                  => devWarning(msg(WildcardType)) ; jsig(ObjectClass.tpe)
        case tp                            =>
          val etp = erasure(member)(tp)
          if (etp eq tp) throw new UnknownSig(tp)
          else jsig(etp)
      }
    }
    class UnknownSig(val tp: Type) extends Exception {
      override def toString = msg(tp)
    }
  }


  private def newCalculator(member: Symbol, owner: Symbol): String = {
    val calc = new SignatureMap(member, owner)
    calc.signature
  }

  private def verifySignature(sym: Symbol, sig: String): Boolean = {
    def wrap(op: => Unit) = {
      try   { op; true }
      catch { case _: Throwable => false }
    }
    // Run the signature parser to catch bogus signatures.
    val isValidSignature = wrap {
      // Alternative: scala.tools.reflect.SigParser (frontend to sun.reflect.generics.parser.SignatureParser)
      import scala.tools.asm.util.CheckClassAdapter
      if (sym.isMethod)    { CheckClassAdapter checkMethodSignature sig } // requires asm-util.jar
      else if (sym.isTerm) { CheckClassAdapter checkFieldSignature  sig }
      else                 { CheckClassAdapter checkClassSignature  sig }
    }

    isValidSignature || {
      currentUnit.warning(sym.pos,
          """|compiler bug: created invalid generic signature for %s in %s
             |signature: %s
             |if this is reproducible, please report bug at https://issues.scala-lang.org/
          """.trim.stripMargin.format(sym, sym.owner.skipPackageObject.fullName, sig))
      false
    }
  }

  private object NeedsSigCollector extends TypeCollector(false) {
    def traverse(tp: Type) {
      if (!result) {
        tp match {
          case st: SubType =>
            traverse(st.supertype)
          case TypeRef(pre, sym, args) =>
            if (sym == ArrayClass) args foreach traverse
            else if (sym.isTypeParameterOrSkolem || sym.isExistentiallyBound || !args.isEmpty) result = true
            else if (sym.isClass) traverse(rebindInnerClass(pre, sym)) // #2585
            else if (!sym.isTopLevel) traverse(pre)
          case PolyType(_, _) | ExistentialType(_, _) =>
            result = true
          case RefinedType(parents, _) =>
            parents foreach traverse
          case ClassInfoType(parents, _, _) =>
            parents foreach traverse
          case AnnotatedType(_, atp, _) =>
            traverse(atp)
          case _ =>
            mapOver(tp)
        }
      }
    }
  }

  override protected def verifyJavaErasure = settings.Xverify.value || settings.debug.value
  def needsJavaSig(tp: Type) = !settings.Ynogenericsig.value && NeedsSigCollector.collect(tp)

  // only refer to type params that will actually make it into the sig, this excludes:
  // * higher-order type parameters
  // * type parameters appearing in method parameters
  // * type members not visible in an enclosing template
  private def isTypeParameterInSig(sym: Symbol, initialSymbol: Symbol) = (
    !sym.isHigherOrderTypeParameter &&
    sym.isTypeParameterOrSkolem && (
      (initialSymbol.enclClassChain.exists(sym isNestedIn _)) ||
      (initialSymbol.isMethod && initialSymbol.typeParams.contains(sym))
    )
  )

  // Ensure every '.' in the generated signature immediately follows
  // a close angle bracket '>'.  Any which do not are replaced with '$'.
  // This arises due to multiply nested classes in the face of the
  // rewriting explained at rebindInnerClass.   This should be done in a
  // more rigorous way up front rather than catching it after the fact,
  // but that will be more involved.
  private def dotCleanup(sig: String): String = {
    var last: Char = '\0'
    sig map {
      case '.' if last != '>' => last = '.' ; '$'
      case ch                 => last = ch ; ch
    }
  }

  /** This object is only used for sanity testing when -check:genjvm is set.
   *  In that case we make sure that the erasure of the `normalized` type
   *  is the same as the erased type that's generated. Normalization means
   *  unboxing some primitive types and further simplifications as they are done in jsig.
   */
  // val prepareSigMap = new TypeMap {
  //   def squashBoxed(tp: Type): Type = tp.dealiasWiden match {
  //     case t @ RefinedType(parents, decls) =>
  //       val parents1 = parents mapConserve squashBoxed
  //       if (parents1 eq parents) tp
  //       else RefinedType(parents1, decls)
  //     case t @ ExistentialType(tparams, tpe) =>
  //       val tpe1 = squashBoxed(tpe)
  //       if (tpe1 eq tpe) t
  //       else ExistentialType(tparams, tpe1)
  //     case t =>
  //       if (boxedClass contains t.typeSymbol) ObjectClass.tpe
  //       else tp
  //   }
  //   def apply(tp: Type): Type = tp.dealiasWiden match {
  //     case tp1 @ TypeBounds(lo, hi) =>
  //       val lo1 = squashBoxed(apply(lo))
  //       val hi1 = squashBoxed(apply(hi))
  //       if ((lo1 eq lo) && (hi1 eq hi)) tp1
  //       else TypeBounds(lo1, hi1)
  //     case tp1 @ TypeRef(pre, sym, args) =>
  //       def argApply(tp: Type) = {
  //         val tp1 = apply(tp)
  //         if (tp1.typeSymbol == UnitClass) ObjectClass.tpe
  //         else squashBoxed(tp1)
  //       }
  //       if (sym == ArrayClass && args.nonEmpty)
  //         if (unboundedGenericArrayLevel(tp1) == 1) ObjectClass.tpe
  //         else mapOver(tp1)
  //       else if (sym == AnyClass || sym == AnyValClass || sym == SingletonClass)
  //         ObjectClass.tpe
  //       else if (sym == UnitClass)
  //         BoxedUnitClass.tpe
  //       else if (sym == NothingClass)
  //         RuntimeNothingClass.tpe
  //       else if (sym == NullClass)
  //         RuntimeNullClass.tpe
  //       else {
  //         val pre1 = apply(pre)
  //         val args1 = args mapConserve argApply
  //         if ((pre1 eq pre) && (args1 eq args)) tp1
  //         else TypeRef(pre1, sym, args1)
  //       }
  //     case tp1 @ MethodType(params, restpe) =>
  //       val params1 = mapOver(params)
  //       val restpe1 = if (restpe.typeSymbol == UnitClass) UnitClass.tpe else apply(restpe)
  //       if ((params1 eq params) && (restpe1 eq restpe)) tp1
  //       else MethodType(params1, restpe1)
  //     case tp1 @ RefinedType(parents, decls) =>
  //       val parents1 = parents mapConserve apply
  //       if (parents1 eq parents) tp1
  //       else RefinedType(parents1, decls)
  //     case t @ ExistentialType(tparams, tpe) =>
  //       val tpe1 = apply(tpe)
  //       if (tpe1 eq tpe) t
  //       else ExistentialType(tparams, tpe1)
  //     case tp1: ClassInfoType =>
  //       tp1
  //     case tp1 =>
  //       mapOver(tp1)
  //   }
  // }

  // if ((settings.check containsName phaseName)) {
  //   val normalizedTpe = enteringErasure(erasure.prepareSigMap(memberTpe))
  //   val bytecodeTpe = owner.thisType.memberInfo(sym)
  //   if (!sym.isType && !sym.isConstructor && !(erasure.erasure(sym)(normalizedTpe) =:= bytecodeTpe)) {
  //     getCurrentCUnit().warning(sym.pos,
  //         """|compiler bug: created generic signature for %s in %s that does not conform to its erasure
  //            |signature: %s
  //            |original type: %s
  //            |normalized type: %s
  //            |erasure type: %s
  //            |if this is reproducible, please report bug at http://issues.scala-lang.org/
  //         """.trim.stripMargin.format(sym, sym.owner.skipPackageObject.fullName, sig, memberTpe, normalizedTpe, bytecodeTpe))
  //      return null
  //   }
  // }


  private def hiBounds(bounds: TypeBounds): List[Type] = bounds.hi.dealiasWiden match {
    case RefinedType(parents, _) => parents map (_.dealiasWiden)
    case tp                      => tp :: Nil
  }

  /** The Java signature of type 'info', for symbol sym. The symbol is used to give the right return
   *  type for constructors.
   */

  // def javaSig(sym0: Symbol, info: Type): Option[String] = enteringErasure {
  //   val isTraitSignature = sym0.enclClass.isTrait

  //   def superSig(parents: List[Type]) = {
  //     val ps = (
  //       if (isTraitSignature) {
  //         // java is unthrilled about seeing interfaces inherit from classes
  //         val ok = parents filter (p => p.typeSymbol.isTrait || p.typeSymbol.isInterface)
  //         // traits should always list Object.
  //         if (ok.isEmpty || ok.head.typeSymbol != ObjectClass) ObjectClass.tpe :: ok
  //         else ok
  //       }
  //       else parents
  //     )
  //     (ps map boxedSig).mkString
  //   }
  //   def boxedSig(tp: Type) = jsig(tp, primitiveOK = false)
  //   def boundsSig(bounds: List[Type]) = {
  //     val (isTrait, isClass) = bounds partition (_.typeSymbol.isTrait)
  //     val classPart = isClass match {
  //       case Nil    => ":" // + boxedSig(ObjectClass.tpe)
  //       case x :: _ => ":" + boxedSig(x)
  //     }
  //     classPart :: (isTrait map boxedSig) mkString ":"
  //   }
  //   def paramSig(tsym: Symbol) = tsym.name + boundsSig(hiBounds(tsym.info.bounds))
  //   def polyParamSig(tparams: List[Symbol]) = (
  //     if (tparams.isEmpty) ""
  //     else tparams map paramSig mkString ("<", "", ">")
  //   )

  //   // Anything which could conceivably be a module (i.e. isn't known to be
  //   // a type parameter or similar) must go through here or the signature is
  //   // likely to end up with Foo<T>.Empty where it needs Foo<T>.Empty$.
  //   def fullNameInSig(sym: Symbol) = "L" + enteringIcode(sym.javaBinaryName)

  //   def jsig(tp0: Type, existentiallyBound: List[Symbol] = Nil, toplevel: Boolean = false, primitiveOK: Boolean = true): String = {
  //     val tp = tp0.dealias
  //     tp match {
  //       case st: SubType =>
  //         jsig(st.supertype, existentiallyBound, toplevel, primitiveOK)
  //       case ExistentialType(tparams, tpe) =>
  //         jsig(tpe, tparams, toplevel, primitiveOK)
  //       case TypeRef(pre, sym, args) =>
  //         def argSig(tp: Type) =
  //           if (existentiallyBound contains tp.typeSymbol) {
  //             val bounds = tp.typeSymbol.info.bounds
  //             if (!(AnyRefClass.tpe <:< bounds.hi)) "+" + boxedSig(bounds.hi)
  //             else if (!(bounds.lo <:< NullClass.tpe)) "-" + boxedSig(bounds.lo)
  //             else "*"
  //           } else {
  //             boxedSig(tp)
  //           }
  //         def classSig = {
  //           val preRebound = pre.baseType(sym.owner) // #2585
  //           dotCleanup(
  //             (
  //               if (needsJavaSig(preRebound)) {
  //                 val s = jsig(preRebound, existentiallyBound)
  //                 if (s.charAt(0) == 'L') s.substring(0, s.length - 1) + "." + sym.javaSimpleName
  //                 else fullNameInSig(sym)
  //               }
  //               else fullNameInSig(sym)
  //             ) + (
  //               if (args.isEmpty) "" else
  //               "<"+(args map argSig).mkString+">"
  //             ) + (
  //               ";"
  //             )
  //           )
  //         }

  //         // If args isEmpty, Array is being used as a type constructor
  //         if (sym == ArrayClass && args.nonEmpty) {
  //           if (unboundedGenericArrayLevel(tp) == 1) jsig(ObjectClass.tpe)
  //           else ARRAY_TAG.toString+(args map (jsig(_))).mkString
  //         }
  //         else if (isTypeParameterInSig(sym, sym0)) {
  //           assert(!sym.isAliasType, "Unexpected alias type: " + sym)
  //           "" + TVAR_TAG + sym.name + ";"
  //         }
  //         else if (sym == AnyClass || sym == AnyValClass || sym == SingletonClass)
  //           jsig(ObjectClass.tpe)
  //         else if (sym == UnitClass)
  //           jsig(BoxedUnitClass.tpe)
  //         else if (sym == NothingClass)
  //           jsig(RuntimeNothingClass.tpe)
  //         else if (sym == NullClass)
  //           jsig(RuntimeNullClass.tpe)
  //         else if (isPrimitiveValueClass(sym)) {
  //           if (!primitiveOK) jsig(ObjectClass.tpe)
  //           else if (sym == UnitClass) jsig(BoxedUnitClass.tpe)
  //           else abbrvTag(sym).toString
  //         }
  //         else if (sym.isDerivedValueClass) {
  //           val unboxed     = sym.derivedValueClassUnbox.info.finalResultType
  //           val unboxedSeen = (tp memberType sym.derivedValueClassUnbox).finalResultType
  //           def unboxedMsg  = if (unboxed == unboxedSeen) "" else s", seen within ${sym.simpleName} as $unboxedSeen"
  //           logResult(s"Erasure of value class $sym (underlying type $unboxed$unboxedMsg) is") {
  //             if (isPrimitiveValueType(unboxedSeen) && !primitiveOK)
  //               classSig
  //             else
  //               jsig(unboxedSeen, existentiallyBound, toplevel, primitiveOK)
  //           }
  //         }
  //         else if (sym.isClass)
  //           classSig
  //         else
  //           jsig(erasure(sym0)(tp), existentiallyBound, toplevel, primitiveOK)
  //       case PolyType(tparams, restpe) =>
  //         assert(tparams.nonEmpty)
  //         val poly = if (toplevel) polyParamSig(tparams) else ""
  //         poly + jsig(restpe)

  //       case MethodType(params, restpe) =>
  //         val buf = new StringBuffer("(")
  //         params foreach (p => buf append jsig(p.tpe))
  //         buf append ")"
  //         buf append (if (restpe.typeSymbol == UnitClass || sym0.isConstructor) VOID_TAG.toString else jsig(restpe))
  //         buf.toString

  //       case RefinedType(parent :: _, decls) =>
  //         boxedSig(parent)
  //       case ClassInfoType(parents, _, _) =>
  //         superSig(parents)
  //       case AnnotatedType(_, atp, _) =>
  //         jsig(atp, existentiallyBound, toplevel, primitiveOK)
  //       case BoundedWildcardType(bounds) =>
  //         println("something's wrong: "+sym0+":"+sym0.tpe+" has a bounded wildcard type")
  //         jsig(bounds.hi, existentiallyBound, toplevel, primitiveOK)
  //       case _ =>
  //         val etp = erasure(sym0)(tp)
  //         if (etp eq tp) throw new UnknownSig
  //         else jsig(etp)
  //     }
  //   }
  //   if (needsJavaSig(info)) {
  //     try Some(jsig(info, toplevel = true))
  //     catch { case ex: UnknownSig => None }
  //   }
  //   else None
  // }
}
