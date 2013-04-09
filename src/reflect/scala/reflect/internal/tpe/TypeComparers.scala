package scala.reflect
package internal
package tpe

import scala.collection.{ mutable }
import Flags._
import util.Statistics
import scala.annotation.tailrec

trait TypeComparers {
  self: SymbolTable =>
  import definitions._
  import TypesStats._

  private final val LogPendingSubTypesThreshold = DefaultLogThreshhold

  private val pendingSubTypes = new mutable.HashSet[SubTypePair]

  class SubTypePair(val tp1: Type, val tp2: Type) {
    override def hashCode = tp1.hashCode * 41 + tp2.hashCode
    override def equals(other: Any) = (this eq other.asInstanceOf[AnyRef]) || (other match {
      // suspend TypeVars in types compared by =:=,
      // since we don't want to mutate them simply to check whether a subtype test is pending
      // in addition to making subtyping "more correct" for type vars,
      // it should avoid the stackoverflow that's been plaguing us (https://groups.google.com/d/topic/scala-internals/2gHzNjtB4xA/discussion)
      // this method is only called when subtyping hits a recursion threshold (subsametypeRecursions >= LogPendingSubTypesThreshold)
      case stp: SubTypePair =>
        val tvars = List(tp1, stp.tp1, tp2, stp.tp2) flatMap (t => if (t.isGround) Nil else typeVarsInType(t))
        suspendingTypeVars(tvars)(tp1 =:= stp.tp1 && tp2 =:= stp.tp2)
      case _ =>
        false
    })
    override def toString = tp1+" <:<? "+tp2
  }

  private var subsametypeRecursions: Int = 0

  private def isUnifiable(pre1: Type, pre2: Type) =
    (beginsWithTypeVarOrIsRefined(pre1) || beginsWithTypeVarOrIsRefined(pre2)) && (pre1 =:= pre2)

  /** Returns true iff we are past phase specialize,
    *  sym1 and sym2 are two existential skolems with equal names and bounds,
    *  and pre1 and pre2 are equal prefixes
    */
  private def isSameSpecializedSkolem(sym1: Symbol, sym2: Symbol, pre1: Type, pre2: Type) = {
    sym1.isExistentialSkolem && sym2.isExistentialSkolem &&
      sym1.name == sym2.name &&
      phase.specialized &&
      sym1.info =:= sym2.info &&
      pre1 =:= pre2
  }

  private def isSubPre(pre1: Type, pre2: Type, sym: Symbol) =
    if ((pre1 ne pre2) && (pre1 ne NoPrefix) && (pre2 ne NoPrefix) && pre1 <:< pre2) {
      if (settings.debug) println(s"new isSubPre $sym: $pre1 <:< $pre2")
      true
    } else
      false

  private def equalSymsAndPrefixes(sym1: Symbol, pre1: Type, sym2: Symbol, pre2: Type): Boolean = (
    if (sym1 == sym2)
      sym1.hasPackageFlag || sym1.owner.hasPackageFlag || phase.erasedTypes || pre1 =:= pre2
    else
      (sym1.name == sym2.name) && isUnifiable(pre1, pre2)
  )


  def isDifferentType(tp1: Type, tp2: Type): Boolean = try {
    subsametypeRecursions += 1
    undoLog undo { // undo type constraints that arise from operations in this block
      !isSameType1(tp1, tp2)
    }
  } finally {
    subsametypeRecursions -= 1
    // XXX AM TODO: figure out when it is safe and needed to clear the log -- the commented approach below is too eager (it breaks #3281, #3866)
    // it doesn't help to keep separate recursion counts for the three methods that now share it
    // if (subsametypeRecursions == 0) undoLog.clear()
  }

  def isDifferentTypeConstructor(tp1: Type, tp2: Type) = !isSameTypeConstructor(tp1, tp2)

  private def isSameTypeConstructor(tr1: TypeRef, tr2: TypeRef): Boolean = (
       (tr1.sym == tr2.sym)
    && !isDifferentType(tr1.pre, tr2.pre)
  )
  private def isSameTypeConstructor(tp1: Type, tp2: Type): Boolean = (
       tp1.isInstanceOf[TypeRef]
    && tp2.isInstanceOf[TypeRef]
    && isSameTypeConstructor(tp1.asInstanceOf[TypeRef], tp2.asInstanceOf[TypeRef])
  )

  private def isSameType1(tp1: Type, tp2: Type): Boolean = {
    if ((tp1 eq tp2) ||
      (tp1 eq ErrorType) || (tp1 eq WildcardType) ||
      (tp2 eq ErrorType) || (tp2 eq WildcardType))
      true
    else if ((tp1 eq NoType) || (tp2 eq NoType))
      false
    else if (tp1 eq NoPrefix) // !! I do not see how this would be warranted by the spec
      tp2.typeSymbol.isPackageClass
    else if (tp2 eq NoPrefix) // !! I do not see how this would be warranted by the spec
      tp1.typeSymbol.isPackageClass
    else if (tp1.isInstanceOf[AnnotatedType] || tp2.isInstanceOf[AnnotatedType])
      annotationsConform(tp1, tp2) && annotationsConform(tp2, tp1) && (tp1.withoutAnnotations =:= tp2.withoutAnnotations)
    else {
      // We flush out any AnnotatedTypes before calling isSameType2 because
      // unlike most other subclasses of Type, we have to allow for equivalence of any
      // combination of { tp1, tp2 } { is, is not } an AnnotatedType - this because the
      // logic of "annotationsConform" is arbitrary and unknown.
      isSameType2(tp1, tp2) || {
        val tp1n = normalizePlus(tp1)
        val tp2n = normalizePlus(tp2)
        ((tp1n ne tp1) || (tp2n ne tp2)) && isSameType(tp1n, tp2n)
      }
    }
  }

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

  private def isSubOrSameMethodType(tp1: MethodType, tp2: MethodType, requireSame: Boolean): Boolean = {
    val MethodType(params1, res1) = tp1
    val MethodType(params2, res2) = tp2
    def res1subst                 = res1.substSym(params1, params2)
    def resultTypesOk             = if (requireSame) res1subst =:= res2 else res1subst <:< res2

    (    sameLength(params1, params2)
      && tp1.isImplicit == tp2.isImplicit
      && matchingParams(params1, params2, tp1.isJava, tp2.isJava)
      && resultTypesOk
    )
  }

  private def isSameMethodType(mt1: MethodType, mt2: MethodType) = (
       isSameTypes(mt1.paramTypes, mt2.paramTypes)
    && (mt1.resultType =:= mt2.resultType.substSym(mt2.params, mt1.params))
    && (mt1.isImplicit == mt2.isImplicit)
  )

  // If Ti ≡ Ti′ for i = 1, ..., n and U conforms to U′, then the method
  // type (p1: T1, ..., pn: Tn)U conforms to (p1′:T1′, ..., pn′: Tn′)U′
  // private def isSameMethodType(tp1: MethodType, tp2: MethodType) =
  //   isSubOrSameMethodType(tp1, tp2, requireSame = true)

  private def isSubMethodType(tp1: MethodType, tp2: MethodType) =
    isSubOrSameMethodType(tp1, tp2, requireSame = false)

  private def equalTypeParamsAndResult(tparams1: List[Symbol], res1: Type, tparams2: List[Symbol], res2: Type) = {
    def subst(info: Type) = info.substSym(tparams2, tparams1)
    // corresponds does not check length of two sequences before checking the predicate,
    // but SubstMap assumes it has been checked (SI-2956)
    (     sameLength(tparams1, tparams2)
      && (tparams1 corresponds tparams2)((p1, p2) => p1.info =:= subst(p2.info))
      && (res1 =:= subst(res2))
    )
  }

  def isSameType2(tp1: Type, tp2: Type): Boolean = {
    /** Here we highlight those unfortunate type-like constructs which
     *  are hidden bundles of mutable state, cruising the type system picking
     *  up any type constraints naive enough to get into their hot rods.
     */
    def mutateNonTypeConstructs(lhs: Type, rhs: Type) = lhs match {
      case BoundedWildcardType(bounds)         => bounds containsType rhs
      case tv @ TypeVar(_, _)                  => tv.registerTypeEquality(rhs, typeVarLHS = lhs eq tp1)
      case TypeRef(tv @ TypeVar(_, _), sym, _) => tv.registerTypeSelection(sym, rhs)
      case _                                   => false
    }
    /*  SingletonType receives this additional scrutiny because there are
     *  a variety of Types which must be treated as equivalent even if they
     *  arrive in different guises. For instance, object Foo in the following
     *  might appear in (at least) the four given below.
     *
     *    package pkg { object Foo ; type Bar = Foo.type }
     *
     *  ModuleClassTypeRef(pkg.type, Foo: ModuleClassSymbol, Nil)
     *  ThisType(Foo: ModuleClassSymbol)
     *  SingleType(pkg.type, Foo: ModuleSymbol)
     *  AliasTypeRef(NoPrefix, sym: AliasSymbol, Nil) where sym.info is one of the above
     */
    def sameSingletonType = tp1 match {
      case tp1: SingletonType => tp2 match {
        case tp2: SingletonType => isSameSingletonType(tp1, tp2)
        case _                  => false
      }
      case _ => false
    }
    /** Those false cases certainly are ugly. There's a proposed SIP to deuglify it.
     *    https://docs.google.com/a/improving.org/document/d/1onPrzSqyDpHScc9PS_hpxJwa3FlPtthxw-bAuuEe8uA
     */
    def sameTypeAndSameCaseClass = tp1 match {
      case tp1: TypeRef               => tp2 match { case tp2: TypeRef               => isSameTypeRef(tp1, tp2)                              ; case _ => false }
      case tp1: MethodType            => tp2 match { case tp2: MethodType            => isSameMethodType(tp1, tp2)                           ; case _ => false }
      case RefinedType(ps1, decls1)   => tp2 match { case RefinedType(ps2, decls2)   => isSameTypes(ps1, ps2) && (decls1 isSameScope decls2) ; case _ => false }
      case SingleType(pre1, sym1)     => tp2 match { case SingleType(pre2, sym2)     => equalSymsAndPrefixes(sym1, pre1, sym2, pre2)         ; case _ => false }
      case PolyType(ps1, res1)        => tp2 match { case PolyType(ps2, res2)        => equalTypeParamsAndResult(ps1, res1, ps2, res2)       ; case _ => false }
      case ExistentialType(qs1, res1) => tp2 match { case ExistentialType(qs2, res2) => equalTypeParamsAndResult(qs1, res1, qs2, res2)       ; case _ => false }
      case ThisType(sym1)             => tp2 match { case ThisType(sym2)             => sym1 == sym2                                         ; case _ => false }
      case ConstantType(c1)           => tp2 match { case ConstantType(c2)           => c1 == c2                                             ; case _ => false }
      case NullaryMethodType(res1)    => tp2 match { case NullaryMethodType(res2)    => res1 =:= res2                                        ; case _ => false }
      case TypeBounds(lo1, hi1)       => tp2 match { case TypeBounds(lo2, hi2)       => lo1 =:= lo2 && hi1 =:= hi2                           ; case _ => false }
      case _                          => false
    }

    (    sameTypeAndSameCaseClass
      || sameSingletonType
      || mutateNonTypeConstructs(tp1, tp2)
      || mutateNonTypeConstructs(tp2, tp1)
    )
  }

  @inline private def lockUndolog(body: => Boolean): Boolean = {
    undoLog.lock()
    var result = false
    def maybeUndo(): Boolean = {
      val saved = undoLog.log
      try result = body
      finally if (!result) undoLog undoTo saved
      result
    }
    try maybeUndo() finally undoLog.unlock()
  }
  // XXX AM TODO: figure out when it is safe and needed to clear the log --
  // the commented approach below is too eager (it breaks #3281, #3866)
  // it doesn't help to keep separate recursion counts for the three methods that now share it
  // if (subsametypeRecursions == 0) undoLog.clear()

  @inline private def withPendingSubtype(p: SubTypePair)(body: => Boolean): Boolean = {
    !pendingSubTypes(p) && {
      pendingSubTypes += p
      try body
      finally pendingSubTypes -= p
    }
  }
  @inline private def incrementSubSame(op: => Boolean): Boolean = {
    subsametypeRecursions += 1
    try lockUndolog(op) finally subsametypeRecursions -= 1
  }

  def isSubType(tp1: Type, tp2: Type): Boolean = isSubType(tp1, tp2, AnyDepth)
  /** Do `tp1` and `tp2` denote equivalent types? */
  def isSameType(tp1: Type, tp2: Type): Boolean = incrementSubSame(isSameType1(tp1, tp2))

  def isSubType(tp1: Type, tp2: Type, depth: Int): Boolean = {
    incrementSubSame {
      if (subsametypeRecursions < LogPendingSubTypesThreshold)
        isSubType2(tp1, tp2, depth)
      else
        withPendingSubtype(new SubTypePair(tp1, tp2))(isSubType2(tp1, tp2, depth))
    }
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

  // private def isShallowSubType

  /** Does type `tp1` conform to `tp2`? */
  private def isSubType2(tp1: Type, tp2: Type, depth: Int): Boolean = /*printResult(s"isSubType2($tp1, $tp2, $depth")*/ {
    def annotatedConforms = (
         annotationsConform(tp1, tp2)
      && isSubType(tp1.withoutAnnotations, tp2.withoutAnnotations, depth)
    )
    if ((tp1 eq tp2) || isErrorOrWildcard(tp1) || isErrorOrWildcard(tp2)) return true
    if ((tp1 eq NoType) || (tp2 eq NoType)) return false
    if (tp1 eq NoPrefix) return (tp2 eq NoPrefix) || tp2.typeSymbol.isPackageClass // !! I do not see how the "isPackageClass" would be warranted by the spec
    if (tp2 eq NoPrefix) return tp1.typeSymbol.isPackageClass
    if (isSingleType(tp1) && isSingleType(tp2) || isConstantType(tp1) && isConstantType(tp2)) return tp1 =:= tp2
    if (tp1.isHigherKinded || tp2.isHigherKinded) return isHKSubType(tp1, tp2, depth)
    if (tp1.isInstanceOf[AnnotatedType] || tp2.isInstanceOf[AnnotatedType])
      return annotatedConforms

    // !! I do not see how the "isPackageClass" would be warranted by the spec
    // if ((tp1 eq tp2) || isErrorOrWildcard(tp1) || isErrorOrWildcard(tp2))
    //   true
    // else if ((tp1 eq NoType) || (tp2 eq NoType))
    //   false
    // else if (tp1 eq NoPrefix)
    //   conformsToNoPrefix(tp2)
    // else if (tp2 eq NoPrefix)
    //   conformsToNoPrefix(tp1)
    // else if (requiresEquivalence(tp1) || requiresEquivalence(tp2))
    //   tp1 =:= tp2
    // else if (tp1.isHigherKinded || tp2.isHigherKinded)
    //   isHKSubType(tp1, tp2, depth)
    tp2 match {
      case BoundedWildcardType(bounds) => return isSubType(tp1, bounds.hi, depth)
      case tv2 @ TypeVar(_, _) =>
        tp1 match {
          case AnnotatedType(_, _, _) | BoundedWildcardType(_) =>
          case _                                               => return tv2.registerBound(tp1, isLowerBound = true)
        }
      case _  =>
    }
    tp1 match {
      case BoundedWildcardType(bounds) => return isSubType(bounds.lo, tp2, depth)
      case tv @ TypeVar(_,_)           => return tv.registerBound(tp2, isLowerBound = false)
      case _                           =>
    }

    isSubType3(tp1, tp2, depth)
  }


  /** Does type `tp1` conform to `tp2`? */
  private def isSubType3(tp1: Type, tp2: Type, depth: Int): Boolean = {
    def isSub(lhs: Type, rhs: Type) = ((lhs ne tp1) || (rhs ne tp2)) && isSubType(lhs, rhs, depth)
    def replaceLeft(lhs: Type)      = (lhs ne tp1) && isSub(lhs, tp2)
    def replaceRight(rhs: Type)     = (rhs ne tp2) && isSub(tp1, rhs)

    def aberrantTypes(tp: Type, canWiden: Boolean): List[Type] = {
      val next = if (typeContainsTypeVar(tp)) null else tp match {
        // case TypeRef(pre, sym, Nil) if sym.isModuleClass            => tp.narrow                       // module classes
        case TypeRef(pre, sym, Nil) if sym.isRefinementClass           => pre memberInfo sym              // refinement classes
        case TypeRef(_, sym, Nil) if isRawIfWithoutArgs(sym)           => rawToExistential(tp)            // raw types
        case TypeRef(pre, sym, args) if sym.isAbstractType && canWiden => sym.info.bounds.hi              // abstract type bounds
        // case TypeRef(pre, sym, args) if sym.isAbstractType && canWiden => (pre memberInfo sym).bounds.hi  // abstract type bounds
        case _ if tp ne tp.dealias                                     => tp.dealias                      // type aliases
        case _: SingletonType if canWiden                              => tp.underlying                   // singleton types
        case _ if tp ne tp.normalize                                   => tp.normalize
        case _                                                         => null
      }
      if (next eq null) Nil else logResult("aberrant/isLhs=" + canWiden)(next) :: aberrantTypes(next, canWiden)
    }
    def aberrantConformance = {
      val lhsTypes = tp1 :: aberrantTypes(tp1, canWiden = true)
      val rhsTypes = tp2 :: aberrantTypes(tp2, canWiden = false)

      /*printResult(s"${lhsTypes.size}/${rhsTypes.size} aberrantConformance(tp1=$tp1, tp2=$tp2)")*/(
        lhsTypes exists (lhs =>
          rhsTypes exists (rhs =>
            { val s1 = s"$lhs/${util.shortClassOfInstance(lhs)}"
              val s2 = s"$rhs/${util.shortClassOfInstance(rhs)}"
              val s3 = f"  $s1%50s    $s2%-50s"
              logResult(s3)(isSub(lhs, rhs))
            }
          )
        )
      )
    }
    def typeRefOnRight(tp1: Type, tp2: TypeRef): Boolean = {
      def lo = tp2.bounds.lo
      tp2.sym match {
        case SingletonClass                    => tp1.isStable
        case _: ClassSymbol                    => false
        case s: TypeSymbol if s.isAbstractType => isDifferentTypeConstructor(tp2, lo) && replaceRight(lo)
        // case _: TypeSymbol                     => isSub(tp1.normalize, tp2.normalize)
        case _                                 => false
      }
    }
    def admitsNull(tp: Type): Boolean = {
      def symAdmitsNull(sym: Symbol): Boolean = (
           (NullClass isBottomSubClass sym)
        || sym.isAbstractType && admitsNull(sym.info.bounds.hi)
      )
      def loop(tps: List[Type]): Boolean = tps match {
        case TypeRef(_, sym, _) :: _ if symAdmitsNull(sym) => true
        case _ :: tl                                       => loop(tl)
        case Nil                                           => false
      }
      loop(tp.dealiasWidenChain)
    }
    def typeRefOnLeft(tp1: TypeRef, tp2: Type): Boolean = {
      def hi = tp1.bounds.hi
      def isNullable = admitsNull(tp2)
      tp1.sym match {
        case NothingClass                  => true
        case NullClass                     => isNullable
        case _: ClassSymbol                => false
        case s: TypeSymbol if s.isDeferred => hi.dealiasWidenChain drop 1 exists replaceLeft
        // case _: TypeSymbol                 => isSub(tp1.normalize, tp2.normalize)
        case _                             => false
      }
    }

    /* First try, on the right:
     *   - unwrap Annotated types, BoundedWildcardTypes,
     *   - bind TypeVars  on the right, if lhs is not Annotated nor BoundedWildcard
     *   - handle common cases for first-kind TypeRefs on both sides as a fast path.
     */
    def twoTypeRefs(tp1: TypeRef, tp2: TypeRef) = {
      val TypeRef(pre1, sym1, args1) = tp1
      val TypeRef(pre2, sym2, args2) = tp2

      def compatiblePrefixes = (
           isUnifiable(pre1, pre2)
        || isSameSpecializedSkolem(sym1, sym2, pre1, pre2)
        || sym2.isAbstractType && isSubPre(pre1, pre2, sym2)
      )
      def compatibleSymbols = (
        if (sym1 == sym2)
          phase.erasedTypes || sym1.owner.hasPackageFlag || isSub(pre1, pre2)
        else
          (sym1.name == sym2.name) && compatiblePrefixes
      )
      def compatibleTypeArgs = isSubArgs(args1, args2, sym1.typeParams, depth)

      (    compatibleSymbols && compatibleTypeArgs
        || sym2.isClass && replaceLeft(tp1 baseType sym2)
      )
    }
    def subTypeRequiresSameCaseClass = tp1 match {
      case tp1: MethodType         => tp2 match { case tp2: MethodType         => isSubMethodType(tp1, tp2)          ; case _ => false }
      case TypeBounds(lo1, hi1)    => tp2 match { case TypeBounds(lo2, hi2)    => isSub(lo2, lo1) && isSub(hi1, hi2) ; case _ => false }
      case NullaryMethodType(res1) => tp2 match { case NullaryMethodType(res2) => isSub(res1, res2)                  ; case _ => false }
      case _                       => false
    }

    def fromLeft = tp1 match {
      case tp1: ExistentialType    => atHigherSkolemization(replaceLeft(tp1.skolemizeExistential))
      case tp1: TypeRef            => typeRefOnLeft(tp1, tp2)
      case RefinedType(parents, _) => parents exists replaceLeft
      case _: SingletonType        => replaceLeft(tp1.underlying)
      case _                       => false
    }
    def fromRight = tp2 match {
      case tp2: TypeRef =>
        tp1 match {
          case tp1: TypeRef => twoTypeRefs(tp1, tp2) || typeRefOnRight(tp1, tp2)
          case _            => typeRefOnRight(tp1, tp2)
        }
      case RefinedType(parents, decls) =>
        (parents forall replaceRight) && decls.forall(d => specializesSym(tp1, d, depth))
      case et2: ExistentialType =>
        et2.withTypeVars(replaceRight, depth)
      case _ =>
        false
    }

    (    subTypeRequiresSameCaseClass
      || fromRight
      || fromLeft
      || aberrantConformance
    )
  }


  def isWeakSubType(tp1: Type, tp2: Type) =
    tp1.dealiasWiden match {
      case TypeRef(_, sym1, _) if isNumericValueClass(sym1) =>
        tp2.deconst.dealias match {
          case TypeRef(_, sym2, _) if isNumericValueClass(sym2) =>
            isNumericSubClass(sym1, sym2)
          case tv2 @ TypeVar(_, _) =>
            tv2.registerBound(tp1, isLowerBound = true, isNumericBound = true)
          case _ =>
            isSubType(tp1, tp2)
        }
      case tv1 @ TypeVar(_, _) =>
        tp2.deconst.dealias match {
          case TypeRef(_, sym2, _) if isNumericValueClass(sym2) =>
            tv1.registerBound(tp2, isLowerBound = false, isNumericBound = true)
          case _ =>
            isSubType(tp1, tp2)
        }
      case _ =>
        isSubType(tp1, tp2)
    }

  def isNumericSubType(tp1: Type, tp2: Type) = (
    isNumericSubClass(primitiveBaseClass(tp1.dealiasWiden), primitiveBaseClass(tp2.dealias))
   )

  /** If the given type has a primitive class among its base classes,
   *  the symbol of that class. Otherwise, NoSymbol.
   */
  private def primitiveBaseClass(tp: Type): Symbol = {
    @tailrec def loop(bases: List[Symbol]): Symbol = bases match {
      case Nil     => NoSymbol
      case x :: xs => if (isPrimitiveValueClass(x)) x else loop(xs)
    }
    loop(tp.baseClasses)
  }
}
