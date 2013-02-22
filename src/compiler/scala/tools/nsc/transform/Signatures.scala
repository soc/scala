/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import scala.reflect.internal.ClassfileConstants._
// import scala.collection.{ mutable, immutable }
// import symtab._
// import Flags._
// import scala.reflect.internal.Mode._

trait Signatures extends scala.reflect.internal.transform.Erasure {
  val global: Global

  import global._
  import definitions._

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

  private def hiBounds(bounds: TypeBounds): List[Type] = bounds.hi.dealiasWiden match {
    case RefinedType(parents, _) => parents map (_.dealiasWiden)
    case tp                      => tp :: Nil
  }

  /** The Java signature of type 'info', for symbol sym. The symbol is used to give the right return
   *  type for constructors.
   */
  def javaSig(sym0: Symbol, info: Type): Option[String] = enteringErasure {
    val isTraitSignature = sym0.enclClass.isTrait

    def superSig(parents: List[Type]) = {
      val ps = (
        if (isTraitSignature) {
          // java is unthrilled about seeing interfaces inherit from classes
          val ok = parents filter (p => p.typeSymbol.isTrait || p.typeSymbol.isInterface)
          // traits should always list Object.
          if (ok.isEmpty || ok.head.typeSymbol != ObjectClass) ObjectClass.tpe :: ok
          else ok
        }
        else parents
      )
      (ps map boxedSig).mkString
    }
    def boxedSig(tp: Type) = jsig(tp, primitiveOK = false)
    def boundsSig(bounds: List[Type]) = {
      val (isTrait, isClass) = bounds partition (_.typeSymbol.isTrait)
      val classPart = isClass match {
        case Nil    => ":" // + boxedSig(ObjectClass.tpe)
        case x :: _ => ":" + boxedSig(x)
      }
      classPart :: (isTrait map boxedSig) mkString ":"
    }
    def paramSig(tsym: Symbol) = tsym.name + boundsSig(hiBounds(tsym.info.bounds))
    def polyParamSig(tparams: List[Symbol]) = (
      if (tparams.isEmpty) ""
      else tparams map paramSig mkString ("<", "", ">")
    )

    // Anything which could conceivably be a module (i.e. isn't known to be
    // a type parameter or similar) must go through here or the signature is
    // likely to end up with Foo<T>.Empty where it needs Foo<T>.Empty$.
    def fullNameInSig(sym: Symbol) = "L" + enteringIcode(sym.javaBinaryName)

    def jsig(tp0: Type, existentiallyBound: List[Symbol] = Nil, toplevel: Boolean = false, primitiveOK: Boolean = true): String = {
      val tp = tp0.dealias
      tp match {
        case st: SubType =>
          jsig(st.supertype, existentiallyBound, toplevel, primitiveOK)
        case ExistentialType(tparams, tpe) =>
          jsig(tpe, tparams, toplevel, primitiveOK)
        case TypeRef(pre, sym, args) =>
          def argSig(tp: Type) =
            if (existentiallyBound contains tp.typeSymbol) {
              val bounds = tp.typeSymbol.info.bounds
              if (!(AnyRefClass.tpe <:< bounds.hi)) "+" + boxedSig(bounds.hi)
              else if (!(bounds.lo <:< NullClass.tpe)) "-" + boxedSig(bounds.lo)
              else "*"
            } else {
              boxedSig(tp)
            }
          def classSig = {
            val preRebound = pre.baseType(sym.owner) // #2585
            dotCleanup(
              (
                if (needsJavaSig(preRebound)) {
                  val s = jsig(preRebound, existentiallyBound)
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
          if (sym == ArrayClass && args.nonEmpty) {
            if (unboundedGenericArrayLevel(tp) == 1) jsig(ObjectClass.tpe)
            else ARRAY_TAG.toString+(args map (jsig(_))).mkString
          }
          else if (isTypeParameterInSig(sym, sym0)) {
            assert(!sym.isAliasType, "Unexpected alias type: " + sym)
            "" + TVAR_TAG + sym.name + ";"
          }
          else if (sym == AnyClass || sym == AnyValClass || sym == SingletonClass)
            jsig(ObjectClass.tpe)
          else if (sym == UnitClass)
            jsig(BoxedUnitClass.tpe)
          else if (sym == NothingClass)
            jsig(RuntimeNothingClass.tpe)
          else if (sym == NullClass)
            jsig(RuntimeNullClass.tpe)
          else if (isPrimitiveValueClass(sym)) {
            if (!primitiveOK) jsig(ObjectClass.tpe)
            else if (sym == UnitClass) jsig(BoxedUnitClass.tpe)
            else abbrvTag(sym).toString
          }
          else if (sym.isDerivedValueClass) {
            val unboxed     = sym.derivedValueClassUnbox.info.finalResultType
            val unboxedSeen = (tp memberType sym.derivedValueClassUnbox).finalResultType
            def unboxedMsg  = if (unboxed == unboxedSeen) "" else s", seen within ${sym.simpleName} as $unboxedSeen"
            logResult(s"Erasure of value class $sym (underlying type $unboxed$unboxedMsg) is") {
              if (isPrimitiveValueType(unboxedSeen) && !primitiveOK)
                classSig
              else
                jsig(unboxedSeen, existentiallyBound, toplevel, primitiveOK)
            }
          }
          else if (sym.isClass)
            classSig
          else
            jsig(erasure(sym0)(tp), existentiallyBound, toplevel, primitiveOK)
        case PolyType(tparams, restpe) =>
          assert(tparams.nonEmpty)
          val poly = if (toplevel) polyParamSig(tparams) else ""
          poly + jsig(restpe)

        case MethodType(params, restpe) =>
          val buf = new StringBuffer("(")
          params foreach (p => buf append jsig(p.tpe))
          buf append ")"
          buf append (if (restpe.typeSymbol == UnitClass || sym0.isConstructor) VOID_TAG.toString else jsig(restpe))
          buf.toString

        case RefinedType(parent :: _, decls) =>
          boxedSig(parent)
        case ClassInfoType(parents, _, _) =>
          superSig(parents)
        case AnnotatedType(_, atp, _) =>
          jsig(atp, existentiallyBound, toplevel, primitiveOK)
        case BoundedWildcardType(bounds) =>
          println("something's wrong: "+sym0+":"+sym0.tpe+" has a bounded wildcard type")
          jsig(bounds.hi, existentiallyBound, toplevel, primitiveOK)
        case _ =>
          val etp = erasure(sym0)(tp)
          if (etp eq tp) throw new UnknownSig
          else jsig(etp)
      }
    }
    if (needsJavaSig(info)) {
      try Some(jsig(info, toplevel = true))
      catch { case ex: UnknownSig => None }
    }
    else None
  }

  class UnknownSig extends Exception
}
