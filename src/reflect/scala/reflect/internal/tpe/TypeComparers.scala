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

  /** Do `tp1` and `tp2` denote equivalent types? */
  def isSameType(tp1: Type, tp2: Type): Boolean = try {
    if (Statistics.canEnable) Statistics.incCounter(sametypeCount)
    subsametypeRecursions += 1
    //OPT cutdown on Function0 allocation
    //was:
    //    undoLog undoUnless {
    //      isSameType1(tp1, tp2)
    //    }

    undoLog.lock()
    try {
      val before = undoLog.log
      var result = false
      try {
        result = isSameType1(tp1, tp2)
      }
      finally if (!result) undoLog.undoTo(before)
      result
    }
    finally undoLog.unlock()
  }
  finally {
    subsametypeRecursions -= 1
    // XXX AM TODO: figure out when it is safe and needed to clear the log -- the commented approach below is too eager (it breaks #3281, #3866)
    // it doesn't help to keep separate recursion counts for the three methods that now share it
    // if (subsametypeRecursions == 0) undoLog.clear()
  }

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

  def isSubType(tp1: Type, tp2: Type): Boolean = isSubType(tp1, tp2, AnyDepth)

  def isSubType(tp1: Type, tp2: Type, depth: Int): Boolean = try {
    subsametypeRecursions += 1

    //OPT cutdown on Function0 allocation
    //was:
    //    undoLog undoUnless { // if subtype test fails, it should not affect constraints on typevars
    //      if (subsametypeRecursions >= LogPendingSubTypesThreshold) {
    //        val p = new SubTypePair(tp1, tp2)
    //        if (pendingSubTypes(p))
    //          false
    //        else
    //          try {
    //            pendingSubTypes += p
    //            isSubType2(tp1, tp2, depth)
    //          } finally {
    //            pendingSubTypes -= p
    //          }
    //      } else {
    //        isSubType2(tp1, tp2, depth)
    //      }
    //    }

    undoLog.lock()
    try {
      val before = undoLog.log
      var result = false

      try result = { // if subtype test fails, it should not affect constraints on typevars
        if (subsametypeRecursions >= LogPendingSubTypesThreshold) {
          val p = new SubTypePair(tp1, tp2)
          if (pendingSubTypes(p))
            false
          else
            try {
              pendingSubTypes += p
              isSubType2(tp1, tp2, depth)
            } finally {
              pendingSubTypes -= p
            }
        } else {
          isSubType2(tp1, tp2, depth)
        }
      } finally if (!result) undoLog.undoTo(before)

      result
    } finally undoLog.unlock()
  } finally {
    subsametypeRecursions -= 1
    // XXX AM TODO: figure out when it is safe and needed to clear the log -- the commented approach below is too eager (it breaks #3281, #3866)
    // it doesn't help to keep separate recursion counts for the three methods that now share it
    // if (subsametypeRecursions == 0) undoLog.clear()
  }
  /** Does type `tp1` conform to `tp2`? */
  private def isSubType2(tp1: Type, tp2: Type, depth: Int): Boolean = {
    if ((tp1 eq tp2) || isErrorOrWildcard(tp1) || isErrorOrWildcard(tp2)) return true
    if ((tp1 eq NoType) || (tp2 eq NoType)) return false
    if (tp1 eq NoPrefix) return (tp2 eq NoPrefix) || tp2.typeSymbol.isPackageClass // !! I do not see how the "isPackageClass" would be warranted by the spec
    if (tp2 eq NoPrefix) return tp1.typeSymbol.isPackageClass
    if (isSingleType(tp1) && isSingleType(tp2) || isConstantType(tp1) && isConstantType(tp2)) return tp1 =:= tp2
    if (tp1.isHigherKinded || tp2.isHigherKinded) return isHKSubType(tp1, tp2, depth)

    /* First try, on the right:
     *   - unwrap Annotated types, BoundedWildcardTypes,
     *   - bind TypeVars  on the right, if lhs is not Annotated nor BoundedWildcard
     *   - handle common cases for first-kind TypeRefs on both sides as a fast path.
     */
    def firstTry = tp2 match {
      // fast path: two typerefs, none of them HK
      case tr2: TypeRef =>
        tp1 match {
          case tr1: TypeRef =>
            // TODO - dedicate a method to TypeRef/TypeRef subtyping.
            // These typerefs are pattern matched up and down far more
            // than is necessary.
            val sym1 = tr1.sym
            val sym2 = tr2.sym
            val pre1 = tr1.pre
            val pre2 = tr2.pre
            (((if (sym1 == sym2) phase.erasedTypes || sym1.owner.hasPackageFlag || isSubType(pre1, pre2, depth)
            else (sym1.name == sym2.name && !sym1.isModuleClass && !sym2.isModuleClass &&
              (isUnifiable(pre1, pre2) ||
                isSameSpecializedSkolem(sym1, sym2, pre1, pre2) ||
                sym2.isAbstractType && isSubPre(pre1, pre2, sym2)))) &&
              isSubArgs(tr1.args, tr2.args, sym1.typeParams, depth))
              ||
              sym2.isClass && {
                val base = tr1 baseType sym2
                (base ne tr1) && isSubType(base, tr2, depth)
              }
              ||
              thirdTryRef(tr1, tr2))
          case _ =>
            secondTry
        }
      case AnnotatedType(_, _, _) =>
        isSubType(tp1.withoutAnnotations, tp2.withoutAnnotations, depth) &&
          annotationsConform(tp1, tp2)
      case BoundedWildcardType(bounds) =>
        isSubType(tp1, bounds.hi, depth)
      case tv2 @ TypeVar(_, constr2) =>
        tp1 match {
          case AnnotatedType(_, _, _) | BoundedWildcardType(_) =>
            secondTry
          case _ =>
            tv2.registerBound(tp1, isLowerBound = true)
        }
      case _ =>
        secondTry
    }

    /* Second try, on the left:
     *   - unwrap AnnotatedTypes, BoundedWildcardTypes,
     *   - bind typevars,
     *   - handle existential types by skolemization.
     */
    def secondTry = tp1 match {
      case AnnotatedType(_, _, _) =>
        isSubType(tp1.withoutAnnotations, tp2.withoutAnnotations, depth) &&
          annotationsConform(tp1, tp2)
      case BoundedWildcardType(bounds) =>
        isSubType(tp1.bounds.lo, tp2, depth)
      case tv @ TypeVar(_,_) =>
        tv.registerBound(tp2, isLowerBound = false)
      case ExistentialType(_, _) =>
        try {
          skolemizationLevel += 1
          isSubType(tp1.skolemizeExistential, tp2, depth)
        } finally {
          skolemizationLevel -= 1
        }
      case _ =>
        thirdTry
    }

    def thirdTryRef(tp1: Type, tp2: TypeRef): Boolean = {
      val sym2 = tp2.sym
      def retry(lhs: Type, rhs: Type)   = isSubType(lhs, rhs, depth)
      def abstractTypeOnRight(lo: Type) = isDifferentTypeConstructor(tp2, lo) && retry(tp1, lo)
      def classOnRight                  = (
        if (isRawType(tp2)) retry(tp1, rawToExistential(tp2))
        else if (sym2.isRefinementClass) retry(tp1, sym2.info)
        else fourthTry
      )
      sym2 match {
        case SingletonClass                   => tp1.isStable || fourthTry
        case _: ClassSymbol                   => classOnRight
        case _: TypeSymbol if sym2.isDeferred => abstractTypeOnRight(tp2.bounds.lo) || fourthTry
        case _: TypeSymbol                    => retry(tp1.normalize, tp2.normalize)
        case _                                => fourthTry
      }
    }

    /* Third try, on the right:
     *   - decompose refined types.
     *   - handle typerefs and existentials.
     *   - handle left+right method types, polytypes, typebounds
     */
    def thirdTry = tp2 match {
      case tr2: TypeRef =>
        thirdTryRef(tp1, tr2)
      case rt2: RefinedType =>
        (rt2.parents forall (isSubType(tp1, _, depth))) &&
          (rt2.decls forall (specializesSym(tp1, _, depth)))
      case et2: ExistentialType =>
        et2.withTypeVars(isSubType(tp1, _, depth), depth) || fourthTry
      case mt2: MethodType =>
        tp1 match {
          case mt1 @ MethodType(params1, res1) => isSubMethodType(mt1m, mt2)
          case _                               => false
        }
      case pt2 @ NullaryMethodType(_) =>
        tp1 match {
          // TODO: consider MethodType mt for which mt.params.isEmpty??
          case pt1 @ NullaryMethodType(_) =>
            isSubType(pt1.resultType, pt2.resultType, depth)
          case _ =>
            false
        }
      case TypeBounds(lo2, hi2) =>
        tp1 match {
          case TypeBounds(lo1, hi1) =>
            isSubType(lo2, lo1, depth) && isSubType(hi1, hi2, depth)
          case _ =>
            false
        }
      case _ =>
        fourthTry
    }

    /* Fourth try, on the left:
     *   - handle typerefs, refined types, and singleton types.
     */
    def fourthTry = {
      def retry(lhs: Type, rhs: Type)  = isSubType(lhs, rhs, depth)
      def abstractTypeOnLeft(hi: Type) = isDifferentTypeConstructor(tp1, hi) && retry(hi, tp2)

      tp1 match {
        case tr1 @ TypeRef(pre1, sym1, _) =>
          def nullOnLeft = tp2 match {
            case TypeRef(_, sym2, _) => sym1 isBottomSubClass sym2
            case _                   => isSingleType(tp2) && retry(tp1, tp2.widen)
          }
          def moduleOnLeft = tp2 match {
            case SingleType(pre2, sym2) => equalSymsAndPrefixes(sym1.sourceModule, pre1, sym2, pre2)
            case _                      => false
          }
          def classOnLeft = (
            if (isRawType(tp1)) retry(rawToExistential(tp1), tp2)
            else if (sym1.isModuleClass) moduleOnLeft
            else sym1.isRefinementClass && retry(sym1.info, tp2)
          )
          sym1 match {
            case NothingClass                     => true
            case NullClass                        => nullOnLeft
            case _: ClassSymbol                   => classOnLeft
            case _: TypeSymbol if sym1.isDeferred => abstractTypeOnLeft(tp1.bounds.hi)
            case _: TypeSymbol                    => retry(tp1.normalize, tp2.normalize)
            case _                                => false
          }
        case RefinedType(parents, _) => parents exists (retry(_, tp2))
        case _: SingletonType        => retry(tp1.underlying, tp2)
        case _                       => false
      }
    }

    firstTry
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
