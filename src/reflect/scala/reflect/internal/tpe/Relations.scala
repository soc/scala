package scala.reflect
package internal
package tpe

import scala.collection.{ mutable }
import Flags._
import util.Statistics
import scala.annotation.tailrec

trait Relations {
  self: SymbolTable =>

  import definitions._
  import TypesStats._

  object Conformance extends TypeRelation {
    def apply(tp1: Type, tp2: Type) = tp1 <:< tp2
  }
  object Equivalence extends TypeRelation {
    def apply(tp1: Type, tp2: Type) = tp1 =:= tp2
  }
  object EquivalenceModuloObject extends TypeRelation {
    def apply(tp1: Type, tp2: Type) = tp1 =:= tp2
  }
  object Matching extends TypeRelation {
    def apply(tp1: Type, tp2: Type) = matchesType(tp1, tp2, alwaysMatchSimple = false)
  }
  object MatchingModuleExistentials extends TypeRelation {
    def apply(tp1: Type, tp2: Type) = matchesType(tp1, tp2, alwaysMatchSimple = true)
  }

  trait TypeRelation {
    def preRelateOnLeft(tp: Type): Type
    def preRelateOnRight(tp: Type): Type
    def isSameType(tp1: Type, tp2: Type): Boolean

    def relateLists(tps1: List[Type], tps2: List[Type]): Boolean
    def relateMethodTypes(tp1: MethodType, tp2: MethodType): Boolean
    def relatePolyTypes(tp1: PolyType, tp2: PolyType): Boolean
    def relateExistentialTypes(tp1: ExistentialType, tp2: ExistentialType): Boolean
    def relateTypeBounds(tp1: TypeBounds, tp2: TypeBounds): Boolean
    def relateRefinedTypes(tp1: RefinedType, tp2: RefinedType): Boolean
    def relateConstants(const1: Constant, const2: Constant): Boolean
    def relateSingletonTypes(tp1: SingletonType, tp2: SingletonType): Boolean
    def relateScopes(scope1: Scope, scope2: Scope): Boolean

    def relatePrefixAndSymbol(pre1: Type, sym1: Symbol, pre2: Type, sym2: Symbol): Boolean
    def relateValueParamsAndResult(params1: List[Symbol], res1: Type, params2: List[Symbol], res2: Type): Boolean
    def relateTypeParamsAndResult(tparams1: List[Symbol], res1: Type, tparams2: List[Symbol], res2: Type): Boolean
    def relateTypeArgs(params: List[Symbol], args1: List[Type], args2: List[Type]): Boolean

    def relateTypeRefs(tp1: TypeRef, tp2: TypeRef): Boolean
    def relateTypeRefOnLeft(tp1: TypeRef, tp2: Type): Boolean
    def relateTypeRefOnRight(tp1: Type, tp2: TypeRef): Boolean
    def relateOthers(tp1: Type, tp2: Type): Boolean
    def relate(tp1: Type, tp2: Type): Boolean
  }

  abstract class AbsTypeRelation extends TypeRelation {
    def relateSingletonTypes(tp1: SingletonType, tp2: SingletonType): Boolean
    def relateScopes(scope1: Scope, scope2: Scope): Boolean

    def preRelateOnLeft(tp: Type): Type                 = tp
    def preRelateOnRight(tp: Type): Type                = tp
    def isSameTypes(tps1: List[Type], tps2: List[Type]) = (tps1 corrresponds tps2)(isSameType)
    def relateLists(tps1: List[Type], tps2: List[Type]) = (tps1 corrresponds tps2)(relate)

    def relateMethodTypes(tp1: MethodType, tp2: MethodType) = (
         tp1.isImplicit == tp2.isImplicit
      && relateValueParamsAndResult(tp1.params, tp1.resultType, tp2.params, tp2.resultType)
    )
    def relatePolyTypes(tp1: PolyType, tp2: PolyType) = (
        relateTypeParamsAndResult(tp1.typeParams, tp1.resultType, tp2.typeParams, tp2.resultType)
    )
    def relateExistentialTypes(tp1: ExistentialType, tp2: ExistentialType) = (
      relateTypeParamsAndResult(tp1.quantified, tp1.underlying, tp2.quantified, tp2.underlying)
    )
    def relateTypeBounds(tp1: TypeBounds, tp2: TypeBounds) = (
         relate(tp1.hi, tp2.hi)
      && relate(tp2.lo, tp1.lo)
    )
    def relateRefinedTypes(tp1: RefinedType, tp2: RefinedType) = (
         relateLists(tp1.parents, tp2.parents)
      && relateScopes(tp1.decls, tp2.decls)
    )
    def relateConstants(const1: Constant, const2: Constant) = const1 == const2

    def relateTypeRefs(tp1: TypeRef, tp2: TypeRef)                                               =
    def relateTypeRefOnLeft(tp1: TypeRef, tp2: Type)                                             =
    def relateTypeRefOnRight(tp1: Type, tp2: TypeRef)                                            =
    def relateSingletons(tp1: SingletonType, tp2: SingletonType)                                 =
    def relateOthers(tp1: Type, tp2: Type)                                                       =

    def apply(tp1: Type, tp2: Type): Boolean = relate(tp1, tp2)
    def relate(tp1: Type, tp2: Type): Boolean = tp1 match {
      case NullaryMethodType(res1)         => tp2 match { case NullaryMethodType(res2)         => relate(res1, res2)                          ; case _ => false }
      case MethodType(params1, res1)       => tp2 match { case MethodType(params2, res2)       => relateMethods(params1, res1, params2, res2) ; case _ => false }
      case PolyType(tparams1, res1)        => tp2 match { case PolyType(tparams2, res2)        => relatePolys(tparams1, res1, tparams2, res2) ; case _ => false }
      case ExistentialType(tparams1, res1) => tp2 match { case ExistentialType(tparams2, res2) => relatePolys(tparams1, res1, tparams2, res2) ; case _ => false }
      case RefinedType(ps1, decls1)        => tp2 match { case RefinedType(ps2, decls2)        => relateRefineds(ps1, decls1, ps2, decls2)    ; case _ => false }
      case ConstantType(const1)            => tp2 match { case ConstantType(const2)            => relateConstants(const1, const2)             ; case _ => false }
      case TypeBounds(lo1, hi1)            => tp2 match { case TypeBounds(lo2, hi2)            => relateBounds(lo1, hi1, lo2, hi2)            ; case _ => false }
      case _                               => typeRefDispatch(tp1, tp2)
    }

    protected def typeRefDispatch(tp1: Type, tp2: Type): Boolean = tp1 match {
      case tp1: TypeRef =>
        tp2 match {
          case tp2: TypeRef => relateTypeRefs(tp1, tp2)
          case _            => relateTypeRefOnLeft(tp1, tp2)
        }
      case tp1: SingletonType =>
        tp2 match {
          case tp2: SingletonType => relateSingletons(tp1, tp2)
          case tp2: TypeRef       => relateTypeRefOnRight(tp1, tp2)
          case _                  => relateOthers(tp1, tp2)
        }
      case _ =>
        tp2 match {
          case tp2: TypeRef => relateTypeRefOnRight(tp1, tp2)
          case _            => relateOthers(tp1, tp2)
        }
    }

    def substitute(newSymbols: List[Symbol])(tp: Type): Type = tp match {
      case MethodType(params, _)       => tp.substSym( params, newSymbols)
      case PolyType(tparams, _)        => tp.substSym(tparams, newSymbols)
      case ExistentialType(eparams, _) => tp.substSym(eparams, newSymbols)
      case _                           => tp
    }

    def relateValueParamsAndResult(params1: List[Symbol], res1: Type, params2: List[Symbol], res2: Type) = (
         isSameTypes(params1, params2)
      && (mt1.resultType =:= mt2.resultType.substSym(mt2.params, mt1.params))
      && (mt1.isImplicit == mt2.isImplicit)
    )

         isSameTypes(tp1.paramTypes, tp2.paramTypes)
      && relate(result1, result2)
    )

    def relateMethods(tp1: MethodType, tp2: MethodType): Boolean = {
      def result1 = tp1.resultType
      def result2 = tp2.resultType.substSym(tp2.params, tp1.params)

      (    tp1.isImplicit == tp2.isImplicit
        && isSameTypes(tp1.paramTypes, tp2.paramTypes)
        && relate(result1, result2)
      )
    }
    def applyPolyTypes(tp1: PolyType, tp2: PolyType): Boolean = {
      val PolyType(tparams1, res1) = tp1
      val PolyType(tparams2, res2) = tp2
      // corresponds does not check length of two sequences before checking the predicate,
      // but SubstMap assumes it has been checked (SI-2956)
      sameLength(tparams1, tparams2) && {
        // fast-path: polymorphic method type -- type params cannot be captured
        tparams1.head.owner.isMethod

        def isSame(lhs: Type, rhs: Type) = isSameType(substitute(symbols)(lhs), substitute(symbols)(rhs))

        def isSub(lhs: Symbol, rhs: Symbol) = substitute(symbols)(lhs)

        val isMethod = tparams1.head.owner.isMethod
        //@M for an example of why we need to generate fresh symbols otherwise, see neg/tcpoly_ticket2101.scala
        val sharedTypeParams = if (isMethod) tparams1 else cloneSymbols(tparams1)

        def sub1(tp: Type) = if (isMethod) tp else substitute(sharedTypeParams)(tp)

        def sub2(tp: Type)              = substitute(sharedTypeParams)(tp)
         tp.substSym(tparams2, sharedTypeParams)
        def cmp(p1: Symbol, p2: Symbol) = sub2(p2.info) <:< sub1(p1.info)

        (tparams1 corresponds tparams2)(cmp) && relate(sub1(res1), sub2(res2))
      }
    }
  }


  private def isSameMethodType(mt1: MethodType, mt2: MethodType) = (
       isSameTypes(mt1.paramTypes, mt2.paramTypes)
    && (mt1.resultType =:= mt2.resultType.substSym(mt2.params, mt1.params))
    && (mt1.isImplicit == mt2.isImplicit)
  )

  private def equalTypeParamsAndResult(tparams1: List[Symbol], res1: Type, tparams2: List[Symbol], res2: Type) = {
    def subst(info: Type) = info.substSym(tparams2, tparams1)
    (    isSameTypes(tp1.paramTypes, tp2.paramTypes)
      && tp1.isImplicit == tp2.isImplicit
      && resOk
    )
  }

  private def isSameOrSubPolyType(tparams1: List[Symbol], res1: Type, tparams2: List[Symbol], res2: Type, same: Boolean): Boolean = (
    (    isSameTypes(tp1.paramTypes, tp2.paramTypes)
      && tp1.isImplicit == tp2.isImplicit
      && resOk
    )
  }

  private def isSameOrSubPolyType(tparams1: List[Symbol], res1: Type, tparams2: List[Symbol], res2: Type, same: Boolean): Boolean = (
    // corresponds does not check length of two sequences before checking the predicate,
    // but SubstMap assumes it has been checked (SI-2956)
    (     sameLength(tparams1, tparams2)
      && (tparams1 corresponds tparams2)((p1, p2) => p1.info =:= subst(p2.info))
      && (res1 =:= subst(res2))
    )
  }

  /** matchesType above is an optimized version of the following implementation:

    def matchesType2(tp1: Type, tp2: Type, alwaysMatchSimple: Boolean): Boolean = {
      def matchesQuantified(tparams1: List[Symbol], tparams2: List[Symbol], res1: Type, res2: Type): Boolean =
        tparams1.length == tparams2.length &&
        matchesType(res1, res2.substSym(tparams2, tparams1), alwaysMatchSimple)
      (tp1, tp2) match {
        case (MethodType(params1, res1), MethodType(params2, res2)) =>
          params1.length == params2.length && // useful pre-secreening optimization
          matchingParams(params1, params2, tp1.isInstanceOf[JavaMethodType], tp2.isInstanceOf[JavaMethodType]) &&
          matchesType(res1, res2, alwaysMatchSimple) &&
          tp1.isImplicit == tp2.isImplicit
        case (PolyType(tparams1, res1), PolyType(tparams2, res2)) =>
          matchesQuantified(tparams1, tparams2, res1, res2)
        case (NullaryMethodType(rtp1), MethodType(List(), rtp2)) =>
          matchesType(rtp1, rtp2, alwaysMatchSimple)
        case (MethodType(List(), rtp1), NullaryMethodType(rtp2)) =>
          matchesType(rtp1, rtp2, alwaysMatchSimple)
        case (ExistentialType(tparams1, res1), ExistentialType(tparams2, res2)) =>
          matchesQuantified(tparams1, tparams2, res1, res2)
        case (ExistentialType(_, res1), _) if alwaysMatchSimple =>
          matchesType(res1, tp2, alwaysMatchSimple)
        case (_, ExistentialType(_, res2)) if alwaysMatchSimple =>
          matchesType(tp1, res2, alwaysMatchSimple)
        case (NullaryMethodType(rtp1), _) =>
          matchesType(rtp1, tp2, alwaysMatchSimple)
        case (_, NullaryMethodType(rtp2)) =>
          matchesType(tp1, rtp2, alwaysMatchSimple)
        case (MethodType(_, _), _) => false
        case (PolyType(_, _), _)   => false
        case (_, MethodType(_, _)) => false
        case (_, PolyType(_, _))   => false
        case _ =>
          alwaysMatchSimple || tp1 =:= tp2
      }
    }
  */


  /** Are `syms1` and `syms2` parameter lists with pairwise equivalent types? */
  protected[internal] def matchingParams(syms1: List[Symbol], syms2: List[Symbol], syms1isJava: Boolean, syms2isJava: Boolean): Boolean = syms1 match {
    case Nil =>
      syms2.isEmpty
    case sym1 :: rest1 =>
      syms2 match {
        case Nil =>
          false
        case sym2 :: rest2 =>
          val tp1 = sym1.tpe
          val tp2 = sym2.tpe
          (tp1 =:= tp2 ||
           syms1isJava && tp2.typeSymbol == ObjectClass && tp1.typeSymbol == AnyClass ||
           syms2isJava && tp1.typeSymbol == ObjectClass && tp2.typeSymbol == AnyClass) &&
          matchingParams(rest1, rest2, syms1isJava, syms2isJava)
      }
  }


  def isSubMethodType() = {
    val params2 = mt2.params
    val res2 = mt2.resultType
    (sameLength(params1, params2) &&
      mt1.isImplicit == mt2.isImplicit &&
      matchingParams(params1, params2, mt1.isJava, mt2.isJava) &&
      isSubType(res1.substSym(params1, params2), res2, depth)
    )
  // TODO: if mt1.params.isEmpty, consider NullaryMethodType?
  }


  private def isPolySubType(tp1: PolyType, tp2: PolyType): Boolean = {
    val PolyType(tparams1, res1) = tp1
    val PolyType(tparams2, res2) = tp2

    sameLength(tparams1, tparams2) && {
      // fast-path: polymorphic method type -- type params cannot be captured
      val isMethod = tparams1.head.owner.isMethod
      //@M for an example of why we need to generate fresh symbols otherwise, see neg/tcpoly_ticket2101.scala
      val substitutes = if (isMethod) tparams1 else cloneSymbols(tparams1)
      def sub1(tp: Type) = if (isMethod) tp else tp.substSym(tparams1, substitutes)
      def sub2(tp: Type) = tp.substSym(tparams2, substitutes)
      def cmp(p1: Symbol, p2: Symbol) = sub2(p2.info) <:< sub1(p1.info)

      (tparams1 corresponds tparams2)(cmp) && (sub1(res1) <:< sub2(res2))
    }
  }


  /** A function implementing `tp1` matches `tp2`. */
  final def matchesType(tp1: Type, tp2: Type, alwaysMatchSimple: Boolean): Boolean = {
    def matchesQuantified(tparams1: List[Symbol], tparams2: List[Symbol], res1: Type, res2: Type): Boolean = (
      sameLength(tparams1, tparams2) &&
      matchesType(res1, res2.substSym(tparams2, tparams1), alwaysMatchSimple)
    )
    def lastTry =
      tp2 match {
        case ExistentialType(_, res2) if alwaysMatchSimple =>
          matchesType(tp1, res2, alwaysMatchSimple = true)
        case MethodType(_, _) =>
          false
        case PolyType(_, _) =>
          false
        case _ =>
          alwaysMatchSimple || tp1 =:= tp2
      }
    tp1 match {
      case mt1 @ MethodType(params1, res1) =>
        tp2 match {
          case mt2 @ MethodType(params2, res2) =>
            // sameLength(params1, params2) was used directly as pre-screening optimization (now done by matchesQuantified -- is that ok, performancewise?)
            mt1.isImplicit == mt2.isImplicit &&
            matchingParams(params1, params2, mt1.isJava, mt2.isJava) &&
            matchesQuantified(params1, params2, res1, res2)
          case NullaryMethodType(res2) =>
            if (params1.isEmpty) matchesType(res1, res2, alwaysMatchSimple)
            else matchesType(tp1, res2, alwaysMatchSimple)
          case ExistentialType(_, res2) =>
            alwaysMatchSimple && matchesType(tp1, res2, alwaysMatchSimple = true)
          case TypeRef(_, sym, Nil) =>
            params1.isEmpty && sym.isModuleClass && matchesType(res1, tp2, alwaysMatchSimple)
          case _ =>
            false
        }
      case mt1 @ NullaryMethodType(res1) =>
        tp2 match {
          case mt2 @ MethodType(Nil, res2)  => // could never match if params nonEmpty, and !mt2.isImplicit is implied by empty param list
            matchesType(res1, res2, alwaysMatchSimple)
          case NullaryMethodType(res2) =>
            matchesType(res1, res2, alwaysMatchSimple)
          case ExistentialType(_, res2) =>
            alwaysMatchSimple && matchesType(tp1, res2, alwaysMatchSimple = true)
          case TypeRef(_, sym, Nil) if sym.isModuleClass =>
            matchesType(res1, tp2, alwaysMatchSimple)
          case _ =>
            matchesType(res1, tp2, alwaysMatchSimple)
        }
      case PolyType(tparams1, res1) =>
        tp2 match {
          case PolyType(tparams2, res2) =>
            if ((tparams1 corresponds tparams2)(_ eq _))
              matchesType(res1, res2, alwaysMatchSimple)
            else
              matchesQuantified(tparams1, tparams2, res1, res2)
          case ExistentialType(_, res2) =>
            alwaysMatchSimple && matchesType(tp1, res2, alwaysMatchSimple = true)
          case _ =>
            false // remember that tparams1.nonEmpty is now an invariant of PolyType
        }
      case ExistentialType(tparams1, res1) =>
        tp2 match {
          case ExistentialType(tparams2, res2) =>
            matchesQuantified(tparams1, tparams2, res1, res2)
          case _ =>
            if (alwaysMatchSimple) matchesType(res1, tp2, alwaysMatchSimple = true)
            else lastTry
        }
      case TypeRef(_, sym, Nil) if sym.isModuleClass =>
        tp2 match {
          case MethodType(Nil, res2)   => matchesType(tp1, res2, alwaysMatchSimple)
          case NullaryMethodType(res2) => matchesType(tp1, res2, alwaysMatchSimple)
          case _                       => lastTry
        }
      case _ =>
        lastTry
    }
  }


  def isSubArgs(tps1: List[Type], tps2: List[Type], tparams: List[Symbol], depth: Int): Boolean = {
    def isSubArg(t1: Type, t2: Type, variance: Variance) = (
         (variance.isContravariant || isSubType(t1, t2, depth))
      && (variance.isCovariant || isSubType(t2, t1, depth))
    )

    corresponds3(tps1, tps2, tparams map (_.variance))(isSubArg)
  }

  // @assume tp1.isHigherKinded || tp2.isHigherKinded
  def isHKSubType(tp1: Type, tp2: Type, depth: Int): Boolean = {
    def isSub(ntp1: Type, ntp2: Type) = (ntp1.withoutAnnotations, ntp2.withoutAnnotations) match {
      case (TypeRef(_, AnyClass, _), _)                                     => false                    // avoid some warnings when Nothing/Any are on the other side
      case (_, TypeRef(_, NothingClass, _))                                 => false
      case (pt1: PolyType, pt2: PolyType)                                   => isPolySubType(pt1, pt2)  // @assume both .isHigherKinded (both normalized to PolyType)
      case (_: PolyType, MethodType(ps, _)) if ps exists (_.tpe.isWildcard) => false                    // don't warn on HasMethodMatching on right hand side
      case _                                                                =>                          // @assume !(both .isHigherKinded) thus cannot be subtypes
        def tp_s(tp: Type): String = f"$tp%-20s ${util.shortClassOfInstance(tp)}%s"
        devWarning(s"HK subtype check on $tp1 and $tp2, but both don't normalize to polytypes:\n  tp1=${tp_s(ntp1)}\n  tp2=${tp_s(ntp2)}")
        false
    }

    (    tp1.typeSymbol == NothingClass       // @M Nothing is subtype of every well-kinded type
      || tp2.typeSymbol == AnyClass           // @M Any is supertype of every well-kinded type (@PP: is it? What about continuations plugin?)
      || isSub(tp1.normalize, tp2.normalize) && annotationsConform(tp1, tp2)  // @M! normalize reduces higher-kinded case to PolyType's
    )
  }


  private def equalSymsAndPrefixes(sym1: Symbol, pre1: Type, sym2: Symbol, pre2: Type): Boolean = (
    if (sym1 == sym2)
      sym1.hasPackageFlag || sym1.owner.hasPackageFlag || phase.erasedTypes || pre1 =:= pre2
    else
      (sym1.name == sym2.name) && isUnifiable(pre1, pre2)
  )


  private def isSameTypeConstructor(tr1: TypeRef, tr2: TypeRef): Boolean = (
       (tr1.sym == tr2.sym)
    && !isDifferentType(tr1.pre, tr2.pre)
  )
  private def isSameTypeConstructor(tp1: Type, tp2: Type): Boolean = (
       tp1.isInstanceOf[TypeRef]
    && tp2.isInstanceOf[TypeRef]
    && isSameTypeConstructor(tp1.asInstanceOf[TypeRef], tp2.asInstanceOf[TypeRef])
  )

  private def isSameHKTypes(tp1: Type, tp2: Type) = (
       tp1.isHigherKinded
    && tp2.isHigherKinded
    && (tp1.normalize =:= tp2.normalize)
  )
  private def isSameTypeRef(tr1: TypeRef, tr2: TypeRef) = (
       equalSymsAndPrefixes(tr1.sym, tr1.pre, tr2.sym, tr2.pre)
    && (isSameHKTypes(tr1, tr2) || isSameTypes(tr1.args, tr2.args))
  )

  private def isSameSingletonType(tp1: SingletonType, tp2: SingletonType): Boolean = {
    // We don't use dealiasWiden here because we are looking for the SAME type,
    // and widening leads to a less specific type. The logic is along the lines of
    // dealiasAndFollowUnderlyingAsLongAsTheTypeIsEquivalent. This method is only
    // called after a surface comparison has failed, so if chaseDealiasedUnderlying
    // does not produce a type other than tp1 and tp2, return false.
    @tailrec def chaseDealiasedUnderlying(tp: Type): Type = tp.underlying.dealias match {
      case next: SingletonType if tp ne next => chaseDealiasedUnderlying(next)
      case _                                 => tp
    }
    val origin1 = chaseDealiasedUnderlying(tp1)
    val origin2 = chaseDealiasedUnderlying(tp2)
    ((origin1 ne tp1) || (origin2 ne tp2)) && (origin1 =:= origin2)
  }

  private def isSameMethodType(mt1: MethodType, mt2: MethodType) = (
       isSameTypes(mt1.paramTypes, mt2.paramTypes)
    && (mt1.resultType =:= mt2.resultType.substSym(mt2.params, mt1.params))
    && (mt1.isImplicit == mt2.isImplicit)
  )

  private def equalTypeParamsAndResult(tparams1: List[Symbol], res1: Type, tparams2: List[Symbol], res2: Type) = {
    def subst(info: Type) = info.substSym(tparams2, tparams1)
    // corresponds does not check length of two sequences before checking the predicate,
    // but SubstMap assumes it has been checked (SI-2956)
    (     sameLength(tparams1, tparams2)
      && (tparams1 corresponds tparams2)((p1, p2) => p1.info =:= subst(p2.info))
      && (res1 =:= subst(res2))
    )
  }

  def matchesQuantified(tparams1: List[Symbol], res1: Type, tparams2: List[Symbol], res2: Type): Boolean = (
    (    sameLength(tparams1, tparams2)
      mt1.isImplicit == mt2.isImplicit &&
      matchingParams(params1, params2, mt1.isJava, mt2.isJava) &&

      && matchesType(res1, res2.substSym(tparams2, tparams1), alwaysMatchSimple)
    )
  )

}
