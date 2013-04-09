package scala.reflect
package internal
package tpe

import scala.collection.{ mutable }
import Flags._
import util.Statistics
import scala.annotation.tailrec
import scala.reflect.internal.Variance._

trait Relations {
  self: SymbolTable =>

  import definitions._
  import TypesStats._

  // private def equalSymsAndPrefixes(sym1: Symbol, pre1: Type, sym2: Symbol, pre2: Type): Boolean = (
  //   if (sym1 == sym2)
  //     sym1.hasPackageFlag || sym1.owner.hasPackageFlag || phase.erasedTypes || pre1 =:= pre2
  //   else
  //     (sym1.name == sym2.name) && isUnifiable(pre1, pre2)
  // )

  // private def isSameTypeConstructor(tr1: TypeRef, tr2: TypeRef): Boolean = (
  //      (tr1.sym == tr2.sym)
  //   && !isDifferentType(tr1.pre, tr2.pre)
  // )
  // private def isSameTypeConstructor(tp1: Type, tp2: Type): Boolean = (
  //      tp1.isInstanceOf[TypeRef]
  //   && tp2.isInstanceOf[TypeRef]
  //   && isSameTypeConstructor(tp1.asInstanceOf[TypeRef], tp2.asInstanceOf[TypeRef])
  // )

  // private def isSameHKTypes(tp1: Type, tp2: Type) = (
  //      tp1.isHigherKinded
  //   && tp2.isHigherKinded
  //   && (tp1.normalize =:= tp2.normalize)
  // )
  // private def isSameTypeRef(tr1: TypeRef, tr2: TypeRef) = (
  //      equalSymsAndPrefixes(tr1.sym, tr1.pre, tr2.sym, tr2.pre)
  //   && (isSameHKTypes(tr1, tr2) || isSameTypes(tr1.args, tr2.args))
  // )

  // private def isSameSingletonType(tp1: SingletonType, tp2: SingletonType): Boolean = {
  //   // We don't use dealiasWiden here because we are looking for the SAME type,
  //   // and widening leads to a less specific type. The logic is along the lines of
  //   // dealiasAndFollowUnderlyingAsLongAsTheTypeIsEquivalent. This method is only
  //   // called after a surface comparison has failed, so if chaseDealiasedUnderlying
  //   // does not produce a type other than tp1 and tp2, return false.
  //   @tailrec def chaseDealiasedUnderlying(tp: Type): Type = tp.underlying.dealias match {
  //     case next: SingletonType if tp ne next => chaseDealiasedUnderlying(next)
  //     case _                                 => tp
  //   }
  //   val origin1 = chaseDealiasedUnderlying(tp1)
  //   val origin2 = chaseDealiasedUnderlying(tp2)
  //   ((origin1 ne tp1) || (origin2 ne tp2)) && (origin1 =:= origin2)
  // }

  // // @assume tp1.isHigherKinded || tp2.isHigherKinded
  // def isHKSubType(tp1: Type, tp2: Type, depth: Int): Boolean = {
  //   def isSub(ntp1: Type, ntp2: Type) = (ntp1.withoutAnnotations, ntp2.withoutAnnotations) match {
  //     case (TypeRef(_, AnyClass, _), _)                                     => false                    // avoid some warnings when Nothing/Any are on the other side
  //     case (_, TypeRef(_, NothingClass, _))                                 => false
  //     case (pt1: PolyType, pt2: PolyType)                                   => isPolySubType(pt1, pt2)  // @assume both .isHigherKinded (both normalized to PolyType)
  //     case (_: PolyType, MethodType(ps, _)) if ps exists (_.tpe.isWildcard) => false                    // don't warn on HasMethodMatching on right hand side
  //     case _                                                                =>                          // @assume !(both .isHigherKinded) thus cannot be subtypes
  //       def tp_s(tp: Type): String = f"$tp%-20s ${util.shortClassOfInstance(tp)}%s"
  //       devWarning(s"HK subtype check on $tp1 and $tp2, but both don't normalize to polytypes:\n  tp1=${tp_s(ntp1)}\n  tp2=${tp_s(ntp2)}")
  //       false
  //   }

  //   (    tp1.typeSymbol == NothingClass       // @M Nothing is subtype of every well-kinded type
  //     || tp2.typeSymbol == AnyClass           // @M Any is supertype of every well-kinded type (@PP: is it? What about continuations plugin?)
  //     || isSub(tp1.normalize, tp2.normalize) && annotationsConform(tp1, tp2)  // @M! normalize reduces higher-kinded case to PolyType's
  //   )
  // }



  trait TypeRelation {
    def canonicalize(tp: Type): Type
    def isSameType(tp1: Type, tp2: Type): Boolean

    protected def begin(tp1: Type, tp2: Type): Boolean
    protected def search(tp1: Type, tp2: Type): Boolean
    protected def failed(tp1: Type, tp2: Type): Boolean

    def relateMethodTypes(tp1: NullaryMethodType, tp2: NullaryMethodType): Boolean
    def relateMethodTypes(tp1: MethodType, tp2: MethodType): Boolean
    def relatePolyTypes(tp1: PolyType, tp2: PolyType): Boolean
    def relateExistentialTypes(tp1: ExistentialType, tp2: ExistentialType): Boolean
    def relateTypeBounds(tp1: TypeBounds, tp2: TypeBounds): Boolean
    def relateRefinedTypes(tp1: RefinedType, tp2: RefinedType): Boolean
    def relateConstants(const1: Constant, const2: Constant): Boolean
    def relateQuantified(tp1: Type, tp2: Type): Boolean
    // def relateQuantified(tparams1: List[Symbol], res1: Type, tparams2: List[Symbol], res2: Type): Boolean

    def relatePrefixAndSymbol(pre1: Type, sym1: Symbol, pre2: Type, sym2: Symbol): Boolean
    def relateTypeArgs(args1: List[Type], args2: List[Type], tparams: List[Symbol]): Boolean
    def relateScopes(decls1: Scope, decls2: Scope): Boolean

    // def relateTypeRefs(tp1: TypeRef, tp2: TypeRef): Boolean
    // def relateTypeRefOnLeft(tp1: TypeRef, tp2: Type): Boolean
    // def relateTypeRefOnRight(tp1: Type, tp2: TypeRef): Boolean
    // def relateOthers(tp1: Type, tp2: Type): Boolean
  }

  abstract class TypeRelationImpl extends TypeRelation {
              final def apply(tp1: Type, tp2: Type) = /*printResult(f"$tp1%45s   $this%-15s   $tp2%s")*/(begin(tp1, tp2) || failed(tp1, tp2))
    protected final def begin(tp1: Type, tp2: Type) = relateIdenticalTypes(canonicalize(tp1), canonicalize(tp2))
    protected def search(tp1: Type, tp2: Type): Boolean
    protected def failed(tp1: Type, tp2: Type) = false

    def relatePrefixAndSymbol(pre1: Type, sym1: Symbol, pre2: Type, sym2: Symbol) = (
         (pre1 =:= pre2)
      && (sym1 == sym2)
    )
    def relateTypeArgs(args1: List[Type], args2: List[Type], tparams: List[Symbol]) = {
      def relateTypeArg(arg1: Type, arg2: Type, tparam: Symbol) = tparam.variance match {
        case Covariant     => arg1 <:< arg2
        case Contravariant => arg2 <:< arg1
        case _             => isSameType(arg1, arg2)
      }
      corresponds3(args1, args2, tparams)(relateTypeArg)
    }
    def relateScopes(decls1: Scope, decls2: Scope) = decls1 isSameScope decls2

    // printResult(s"$tp1 $this.relateQuantified($tp1, $tp2)") {
    /** @pre tp1 and tp2 are both MethodTypes or both PolyTypes.
     */
    def relateQuantified(tp1: Type, tp2: Type): Boolean = printResult(f"$tp1%45s   $this%-15s/rQ   $tp2%s") {
      val isMethod = tp1.params.nonEmpty
      val syms1    = substitutionTargets(tp1)
      val syms2    = substitutionTargets(tp2)
      val syms3    = if (sameLength(syms1, syms2)) cloneSymbols(syms1) else return false

      def subst(info: Type) = info.substSym(syms1, syms3).substSym(syms2, syms3)
      def oneParam(p1: Symbol, p2: Symbol): Boolean = {
        val ptp1 = subst(p1.info)
        val ptp2 = subst(p2.info)
        if (isMethod) (
             isSameType(ptp1, ptp2)
          || p1.owner.isJavaDefined && matchAsAnyAndObject(p1, p2)
          || p2.owner.isJavaDefined && matchAsAnyAndObject(p2, p1)
        )
        else begin(ptp1, ptp2)
      }
      def checkParams = (syms1 corresponds syms2)(oneParam)
      def checkResult = begin(subst(tp1.resultType), subst(tp2.resultType))

      checkParams && checkResult
    }

    def isSameType(tp1: Type, tp2: Type) = tp1 =:= tp2
    def isSameParamType(sym1: Symbol, sym2: Symbol) = (
         isSameType(sym1.tpeHK, sym2.tpeHK)
      || sym1.owner.isJavaDefined && matchAsAnyAndObject(sym1, sym2)
      || sym2.owner.isJavaDefined && matchAsAnyAndObject(sym2, sym1)
    )

    def relateMethodTypes(tp1: MethodType, tp2: MethodType) = (
      tp1.isImplicit == tp2.isImplicit && (
        if (tp1.params.isEmpty)
          tp2.params.isEmpty && begin(tp1.resultType, tp2.resultType)
        else
          tp2.params.nonEmpty && relateQuantified(tp1, tp2)
      )
    )
    // -      matchesType(res1, res2.substSym(tparams2, tparams1), alwaysMatchSimple)

    def relateMethodTypes(tp1: NullaryMethodType, tp2: NullaryMethodType)  = begin(tp1.resultType, tp2.resultType)
    def relatePolyTypes(tp1: PolyType, tp2: PolyType)                      = relateQuantified(tp1, tp2)
    def relateExistentialTypes(tp1: ExistentialType, tp2: ExistentialType) = relateQuantified(tp1, tp2)
    def relateTypeBounds(tp1: TypeBounds, tp2: TypeBounds)                 = begin(tp1.hi, tp2.hi) && begin(tp2.lo, tp1.lo)
    def relateConstants(const1: Constant, const2: Constant)                = const1 == const2
    def relateRefinedTypes(tp1: RefinedType, tp2: RefinedType)             = (
         (tp1.parents corresponds tp2.parents)(begin)
      && relateScopes(tp1.decls, tp2.decls)
    )
    // def relateTypeRefs(tp1: TypeRef, tp2: TypeRef)               =
    // def relateTypeRefOnLeft(tp1: TypeRef, tp2: Type)             =
    // def relateTypeRefOnRight(tp1: Type, tp2: TypeRef)            =
    private def matchAsAnyAndObject(param1: Symbol, param2: Symbol) = param1.typeConstructor.typeSymbol match {
      case AnyClass | ObjectClass => true
      case _                      => false
    }
    private def substitutionTargets(tp: Type): List[Symbol] = tp match {
      case MethodType(params, _)       => params
      case PolyType(tparams, _)        => tparams
      case ExistentialType(eparams, _) => eparams
      case _                           => Nil
    }
    private def relateIdenticalTypes(tp1: Type, tp2: Type): Boolean = /*printResult(s"relateIdenticalTypes($tp1, $tp1)")*/(tp1 match {
      case ConstantType(const1)   => tp2 match { case ConstantType(const2)    => relateConstants(const1, const2)  ; case _ => false }
      case tp1: NullaryMethodType => tp2 match { case tp2: NullaryMethodType  => relateMethodTypes(tp1, tp2)      ; case _ => false }
      case tp1: MethodType        => tp2 match { case tp2: MethodType         => relateMethodTypes(tp1, tp2)      ; case _ => false }
      case tp1: PolyType          => tp2 match { case tp2: PolyType           => relatePolyTypes(tp1, tp2)        ; case _ => false }
      case tp1: ExistentialType   => tp2 match { case tp2: ExistentialType    => relateExistentialTypes(tp1, tp2) ; case _ => false }
      case tp1: RefinedType       => tp2 match { case tp2: RefinedType        => relateRefinedTypes(tp1, tp2)     ; case _ => false }
      case tp1: TypeBounds        => tp2 match { case tp2: TypeBounds         => relateTypeBounds(tp1, tp2)       ; case _ => false }
      case _                      => search(tp1, tp2)
    })
    // private def dispatch2(tp1: Type, tp2: Type): Boolean = relate(tp1, tp2)
    // private def dispatch2(tp1: Type, tp2: Type): Boolean = tp1 match {
    //   case tp1: TypeRef =>
    //     tp2 match {
    //       case tp2: TypeRef => relateTypeRefs(tp1, tp2)
    //       case _            => relateTypeRefOnLeft(tp1, tp2)
    //     }
    //   case _ =>
    //     tp2 match {
    //       case tp2: TypeRef => relateTypeRefOnRight(tp1, tp2)
    //       case _            => relateOthers(tp1, tp2)
    //     }
    // }
    override def toString = (this.getClass.getName split '.').last.replaceAllLiterally("""$""", "")
  }

  abstract class SubSameTypeCommon extends TypeRelationImpl {
    def canonicalize(tp: Type): Type = tp match {
      case TypeRef(pre, sym, Nil) if sym.isModuleClass => tp.narrow
      case _                                           => tp
    }
  }
  abstract class MatchesTypeCommon extends TypeRelationImpl {
    def canonicalizeNullaryRepresentations(tp: Type): Type = tp match {
      case NullaryMethodType(restpe)                   => restpe
      case MethodType(Nil, restpe)                     => restpe
      case TypeRef(pre, sym, Nil) if sym.isModuleClass => tp
      case _                                           => NoType
    }
    def compareDifferentTypes(tp1: Type, tp2: Type) = (
      canonicalizeNullaryRepresentations(tp1) match {
        case NoType => false
        case tp1    => tp1 =:= canonicalizeNullaryRepresentations(tp2)
      }
    )
    def isMethodOrPoly(tp: Type) = tp match {
      case _: PolyType | _: MethodType | _: NullaryMethodType => true
      case _                                                  => false
    }
    protected def search(tp1: Type, tp2: Type) = (
      compareDifferentTypes(tp1, tp2)
    )
  }

  object Conformance extends SubSameTypeCommon {
    protected def search(tp1: Type, tp2: Type) = tp1 <:< tp2
    override def toString = "<:<"
  }
  object Equivalence extends SubSameTypeCommon {
    protected def search(tp1: Type, tp2: Type) = tp1 =:= tp2
    override def toString = "=:="
  }
  object EquivalenceModuloAny extends SubSameTypeCommon {
    protected def search(tp1: Type, tp2: Type) = (
         (tp1 =:= tp2)
      || (ObjectClass isSubClass tp1.typeSymbol) && (ObjectClass isSubClass tp2.typeSymbol)
    )
    override def toString = "java_=:="
  }


  object MatchesType extends MatchesTypeCommon {
    def canonicalize(tp: Type): Type = tp match {
      case MethodType(Nil, restpe)   => canonicalize(restpe)
      case NullaryMethodType(restpe) => canonicalize(restpe)
      case _                         => tp
    }
    /** Is this type close enough to that type so that members
     *  with the two type would override each other? This requires
     *  one of the following to be true:
     *    - Both types are PolyTypes with the same number of type
     *      parameters, as well as matching result types after renaming
     *      corresponding type parameters
     *    - Both types are (Nullary)MethodTypes with equivalent type parameter
     *      types and matching result types
     *    - Both types are equivalent
     *    - phase.erasedTypes is false and neither is a MethodType or PolyType
     */
    override protected def search(tp1: Type, tp2: Type) = (
         super.search(tp1, tp2)
      || isSameType(tp1, tp2)
      || !phase.erasedTypes && !isMethodOrPoly(tp1) && !isMethodOrPoly(tp2)
    )
    override def toString = "matches"
  }

  /** Same as matches, except that non-method types are always assumed to match. */
  object LooselyMatchesType extends MatchesTypeCommon {
    def canonicalize(tp: Type): Type = tp match {
      case MethodType(Nil, res)    => canonicalize(res)
      case NullaryMethodType(res)  => canonicalize(res)
      case ExistentialType(_, res) => canonicalize(res)
      case _                       => tp
    }
    // protected def search(tp1: Type, tp2: Type) = isSameType(tp1, tp2)
    override def toString = "looselyMatches"
  }
}


  // /** A function implementing `tp1` matches `tp2`. */
  // final def matchesType(tp1: Type, tp2: Type, alwaysMatchSimple: Boolean): Boolean = {
  //   def matchesQuantified(tparams1: List[Symbol], tparams2: List[Symbol], res1: Type, res2: Type): Boolean = (
  //     sameLength(tparams1, tparams2) &&
  //     matchesType(res1, res2.substSym(tparams2, tparams1), alwaysMatchSimple)
  //   )
  //   def lastTry =
  //     tp2 match {
  //       case ExistentialType(_, res2) if alwaysMatchSimple =>
  //         matchesType(tp1, res2, alwaysMatchSimple = true)
  //       case MethodType(_, _) =>
  //         false
  //       case PolyType(_, _) =>
  //         false
  //       case _ =>
  //         alwaysMatchSimple || tp1 =:= tp2
  //     }
  //   tp1 match {
  //     case mt1 @ MethodType(params1, res1) =>
  //       tp2 match {
  //         case mt2 @ MethodType(params2, res2) =>


  //           // sameLength(params1, params2) was used directly as pre-screening optimization (now done by matchesQuantified -- is that ok, performancewise?)
  //           mt1.isImplicit == mt2.isImplicit &&
  //           matchingParams(params1, params2, mt1.isJava, mt2.isJava) &&
  //           relateQuantified(params1, res1, params2, res2)
  //         case NullaryMethodType(res2) =>
  //           if (params1.isEmpty) matchesType(res1, res2, alwaysMatchSimple)
  //           else matchesType(tp1, res2, alwaysMatchSimple)
  //         case ExistentialType(_, res2) =>
  //           alwaysMatchSimple && matchesType(tp1, res2, alwaysMatchSimple = true)
  //         case TypeRef(_, sym, Nil) =>
  //           params1.isEmpty && sym.isModuleClass && matchesType(res1, tp2, alwaysMatchSimple)
  //         case _ =>
  //           false
  //       }
  //     case mt1 @ NullaryMethodType(res1) =>
  //       tp2 match {
  //         case mt2 @ MethodType(Nil, res2)  => // could never match if params nonEmpty, and !mt2.isImplicit is implied by empty param list
  //           matchesType(res1, res2, alwaysMatchSimple)
  //         case NullaryMethodType(res2) =>
  //           matchesType(res1, res2, alwaysMatchSimple)
  //         case ExistentialType(_, res2) =>
  //           alwaysMatchSimple && matchesType(tp1, res2, alwaysMatchSimple = true)
  //         case TypeRef(_, sym, Nil) if sym.isModuleClass =>
  //           matchesType(res1, tp2, alwaysMatchSimple)
  //         case _ =>
  //           matchesType(res1, tp2, alwaysMatchSimple)
  //       }
  //     case PolyType(tparams1, res1) =>
  //       tp2 match {
  //         case PolyType(tparams2, res2) =>
  //           if ((tparams1 corresponds tparams2)(_ eq _))
  //             matchesType(res1, res2, alwaysMatchSimple)
  //           else
  //             matchesQuantified(tparams1, tparams2, res1, res2)
  //         case ExistentialType(_, res2) =>
  //           alwaysMatchSimple && matchesType(tp1, res2, alwaysMatchSimple = true)
  //         case _ =>
  //           false // remember that tparams1.nonEmpty is now an invariant of PolyType
  //       }
  //     case ExistentialType(tparams1, res1) =>
  //       tp2 match {
  //         case ExistentialType(tparams2, res2) =>
  //           matchesQuantified(tparams1, tparams2, res1, res2)
  //         case _ =>
  //           if (alwaysMatchSimple) matchesType(res1, tp2, alwaysMatchSimple = true)
  //           else lastTry
  //       }
  //     case TypeRef(_, sym, Nil) if sym.isModuleClass =>
  //       tp2 match {
  //         case MethodType(Nil, res2)   => matchesType(tp1, res2, alwaysMatchSimple)
  //         case NullaryMethodType(res2) => matchesType(tp1, res2, alwaysMatchSimple)
  //         case _                       => lastTry
  //       }
  //     case _ =>
  //       lastTry
  //   }
  // }
