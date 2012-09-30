/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package typechecker

import scala.collection.{ mutable, immutable }
import scala.collection.mutable.ListBuffer
import scala.util.control.ControlThrowable
import symtab.Flags._
import scala.annotation.tailrec
import Checkability._

/** On pattern matcher checkability:
 *
 *  Consider a pattern match of this form: (x: X) match { case _: P => }
 *
 *  There are four possibilities to consider:
 *     [P1] X will always conform to P
 *     [P2] x will never conform to P
 *     [P3] X <: P if some runtime test is true
 *     [P4] X cannot be checked against P
 *
 *  The first two cases correspond to those when there is enough
 *  static information to say X <: P or that (x ∈ X) ⇒ (x ∉ P).
 *  The fourth case includes unknown abstract types or structural
 *  refinements appearing within a pattern.
 *
 *  The third case is the interesting one.  We designate another type, XR,
 *  which is essentially the intersection of X and |P|, where |P| is
 *  the erasure of P.  If XR <: P, then no warning is emitted.
 *
 *  Examples of how this info is put to use:
 *  sealed trait A[T] ; class B[T] extends A[T]
 *    def f(x: B[Int]) = x match { case _: A[Int] if true => }
 *    def g(x: A[Int]) = x match { case _: B[Int] => }
 *
 *  `f` requires no warning because X=B[Int], P=A[Int], and B[Int] <:< A[Int].
 *  `g` requires no warning because X=A[Int], P=B[Int], XR=B[Int], and B[Int] <:< B[Int].
 *      XR=B[Int] because a value of type A[Int] which is tested to be a B can
 *      only be a B[Int], due to the definition of B (B[T] extends A[T].)
 *
 *  This is something like asSeenFrom, only rather than asking what a type looks
 *  like from the point of view of one of its base classes, we ask what it looks
 *  like from the point of view of one of its subclasses.
 */
trait Checkable {
  self: Analyzer =>

  import global._
  import definitions._
  import CheckabilityChecker._

  private def isConsistentArgs(tparams: List[Symbol], args1: List[Type], args2: List[Type]) = (
    forall3(tparams, args1, args2)((tparam, arg1, arg2) =>
      // if left-hand argument is a typevar, make it compatible with variance
      // this is for more precise pattern matching
      // todo: work this in the spec of this method
      // also: think what happens if there are embedded typevars?
      if (tparam.variance == 0) arg1 =:= arg2
      else arg1 match {
        case tv: TypeVar => if (tparam.variance < 0) arg1 <:< arg2 else arg2 <:< arg1
        case _           => true
      }
    )
  )
  private def isConsistent(tp1: Type, tp2: Type): Boolean = (tp1.dealias, tp2.dealias) match {
    case (TypeRef(pre1, sym1, args1), TypeRef(pre2, sym2, args2)) =>
      (    sym1 == sym2
        && pre1 =:= pre2
        && isConsistentArgs(sym1.typeParams, args1, args2)
      )
    case (et: ExistentialType, _) =>
      et.withTypeVars(isConsistent(_, tp2))
    case (_, et: ExistentialType) =>
      et.withTypeVars(isConsistent(tp1, _))
  }

  def isPopulated(tp1: Type, tp2: Type): Boolean = !isUninhabited(tp1, tp2)

  class TypePair(clazz: Symbol, tp1: Type, tp2: Type) {
    assert(tp1.typeSymbol == clazz && tp2.typeSymbol == clazz, ((tp1, tp2)))

    def tparams    = clazz.typeParams
    def consistent = isConsistent(tp1, tp2)

    def mapArgs[T](f: (Symbol, Type, Type) => T): List[T] = {
      map3(tparams, tp1.typeArgs, tp2.typeArgs)(f)
    }
  }
  class BaseTypeComparison(tp1: Type, tp2: Type) {
    val bases       = tp1.baseClasses filter (tp2.baseClasses contains _)
    def uninhabited = pairs exists (pair => !pair.consistent)
    def pairs       = bases map (bc => new TypePair(bc, tp1 baseType bc, tp2 baseType bc))
    def mapArgs[T](f: (Symbol, Type, Type) => T) = pairs flatMap (_ mapArgs f)
  }

  /** The applied type of class 'to' after inferring anything
   *  possible from the knowledge that 'to' must also be of the
   *  type given in 'from'.
   */
  def propagateKnownTypes(from: Type, to: Symbol): Type = {
    def tparams  = to.typeParams
    val tvars    = tparams map (p => TypeVar(p))
    val tvarType = appliedType(to, tvars: _*)
    val cmp      = new BaseTypeComparison(from, tvarType)
    val ok       = cmp.mapArgs((param, arg1, arg2) => arg1 =:= arg2).forall(x => x)

    val resArgs = tparams zip tvars map {
      case (_, tvar) if tvar.instValid => tvar.constr.inst
      case (tparam, _)                 => tparam.tpe
    }
    appliedType(to, resArgs: _*)
  }

  class CheckabilityChecker(val X0: Type, val P0: Type) {
    val cmp = new BaseTypeComparison(X0, P0)
    val X   = X0.widen
    val P   = P0.widen

    def Xsym = X.typeSymbol
    def Psym = P.typeSymbol
    // sadly the spec says (new java.lang.Boolean(true)).isInstanceOf[scala.Boolean]
    def includesNumeric = isNumericValueClass(Psym) || isNumericValueClass(Xsym)

    def XR   = propagateKnownTypes(X, Psym)
    def P1   = X matchesPattern P
    def P2   = !includesNumeric && cmp.uninhabited
    def P3   = isNonRefinementClassType(P) && (XR matchesPattern P)
    def P4   = !(P1 || P2 || P3)

    def summaryString = f"""
      |Checking checkability of (x: $X) against pattern $P
      |[P1] $P1%-6s X <: P             // $X  <: $P
      |[P2] $P2%-6s x ∉ P              // (x ∈ $X) ⇒ (x ∉ $P)
      |[P3] $P3%-6s XR <: P            // $XR <: $P
      |[P4] $P4%-6s None of the above  // !(P1 || P2 || P3)
    """.stripMargin.trim

    val result = (
      if (X.isErroneous || P.isErroneous) CheckabilityError
      else if (P1) StaticallyTrue
      else if (P2) Uninhabited
      else if (P3) RuntimeCheckable
      else if (uncheckableType == NoType) {
        // Avoid warning (except ourselves) if we can't pinpoint the uncheckable type
        debugwarn("Checkability checker says 'Uncheckable', but uncheckable type cannot be found:\n" + summaryString)
        CheckabilityError
      }
      else Uncheckable
    )
    lazy val uncheckableType = if (Psym.isAbstractType) P else {
      val possibles = typeArgsInTopLevelType(P).toSet
      val opt = possibles find { targ =>
        // Create a derived type with every possibly uncheckable type replaced
        // with a WildcardType, except for 'targ'. If !(XR <: derived) then
        // 'targ' is uncheckable.
        val derived = P map (tp => if (possibles(tp) && !(tp =:= targ)) WildcardType else tp)
        !(XR <:< derived)
      }
      opt getOrElse NoType
    }

    def unrelatable = isUnrelatable(Xsym, Psym)
    def uninhabited = result == Uninhabited
    def uncheckable = result == Uncheckable
    def uncheckableMessage = uncheckableType match {
      case NoType                                   => "something"
      case tp @ RefinedType(_, _)                   => "refinement " + tp
      case TypeRef(_, sym, _) if sym.isAbstractType => "abstract type " + sym.name
      case tp                                       => "non-variable type argument " + tp
    }

    private def isUnwarnableTypeArgSymbol(sym: Symbol) = (
         sym.isTypeParameter                     // dummy
      || (sym.name.toTermName == nme.WILDCARD)   // _
      || nme.isVariableName(sym.name)            // type variable
    )
    private def isUnwarnableTypeArg(arg: Type) = (
         uncheckedOk(arg)                                 // @unchecked T
      || isUnwarnableTypeArgSymbol(arg.typeSymbolDirect)  // has to be direct: see pos/t1439
    )
    private def uncheckedOk(tp: Type) = tp hasAnnotation UncheckedClass

    private def typeArgsInTopLevelType(tp: Type): List[Type] = {
      val tps = tp match {
        case RefinedType(parents, _)              => parents flatMap typeArgsInTopLevelType
        case TypeRef(_, ArrayClass, arg :: Nil)   => typeArgsInTopLevelType(arg)
        case TypeRef(pre, sym, args)              => typeArgsInTopLevelType(pre) ++ args
        case ExistentialType(tparams, underlying) => tparams.map(_.tpe) ++ typeArgsInTopLevelType(underlying)
        case _                                    => Nil
      }
      tps filterNot isUnwarnableTypeArg
    }
  }

  /** X, P, [P1], etc. are all explained at the top of the file.
   */
  object CheckabilityChecker {
    /** A knowable class is one which is either effectively final
     *  itself, or sealed with only knowable children.
     */
    def isKnowable(sym: Symbol): Boolean = /*logResult(s"isKnowable($sym)")*/(
         sym.initialize.isEffectivelyFinal  // pesky .initialize requirement, or we receive lies about isSealed
      || sym.isSealed && (sym.children forall isKnowable)
    )
    def knownSubclasses(sym: Symbol): List[Symbol] = /*logResult(s"knownSubclasses($sym)")*/(sym :: {
      if (sym.isSealed) sym.children.toList flatMap knownSubclasses
      else Nil
    })
    def excludable(s1: Symbol, s2: Symbol) = /*logResult(s"excludable($s1, $s2)")*/(
         isKnowable(s1)
      && !(s2 isSubClass s1)
      && knownSubclasses(s1).forall(child => !(child isSubClass s2))
    )

    /** Given classes A and B, can it be shown that nothing which is
     *  an A will ever be a subclass of something which is a B? This
     *  entails not only showing that !(A isSubClass B) but that the
     *  same is true of all their subclasses.  Restated for symmetry:
     *  the same value cannot be a member of both A and B.
     *
     *   1) A must not be a subclass of B, nor B of A (the trivial check)
     *   2) One of A or B must be completely knowable (see isKnowable)
     *   3) Assuming A is knowable, the proposition is true if
     *      !(A' isSubClass B) for all A', where A' is a subclass of A.
     *
     *  Due to symmetry, the last condition applies as well in reverse.
     */
    def isUnrelatable(sym1: Symbol, sym2: Symbol) = /*logResult(s"isUnrelatable($sym1, $sym2)")*/(
         sym1.isClass
      && sym2.isClass
      && (excludable(sym1, sym2) || excludable(sym2, sym1))
    )

    /** Is intersection of given types populated? That is,
     *  for all types tp1, tp2 in intersection
     *    for all common base classes bc of tp1 and tp2
     *      let bt1, bt2 be the base types of tp1, tp2 relative to class bc
     *      Then:
     *        bt1 and bt2 have the same prefix, and
     *        any corresponding non-variant type arguments of bt1 and bt2 are the same
     */
    def isUninhabited(tp1: Type, tp2: Type): Boolean = ((tp1.dealias, tp2.dealias)) match {
      case (TypeRef(pre1, sym1, args1), TypeRef(pre2, sym2, args2)) =>
        (    isUnrelatable(sym1, sym2)
          || !(pre1 =:= pre2)
          || !isPopulated(tp1, tp2)
          // (sym1 == sym2) &&
          // exists3(sym1.typeParams, args1, args2)(isUninhabited)
        )
      case _ =>
        false
    }
  }

  trait InferCheckable {
    self: Inferencer =>

    import InferErrorGen._

    /** TODO: much better error positions.
     *  Kind of stuck right now because they just pass us the one tree.
     *  TODO: Eliminate inPattern, canRemedy, which have no place here.
     */
    def checkCheckable(tree: Tree, P0: Type, X0: Type, inPattern: Boolean, canRemedy: Boolean = false): Boolean = {
      if (uncheckedOk(P0)) return true
      val checker = new CheckabilityChecker(X0, P0)
      import checker.{ X, P }

      if (checker.uninhabited) {
        IncompatibleScrutineeTypeError(tree, P0, X0)
        return false
      }

      P match {
        // Prohibit top-level type tests for these, but they are ok nested (e.g. case Foldable[Nothing] => ... )
        case TypeRef(_, NothingClass | NullClass | AnyValClass, _) =>
          TypePatternOrIsInstanceTestError(tree, P)
        // If top-level abstract types can be checked using a classtag extractor, don't warn about them
        case TypeRef(_, sym, _) if sym.isAbstractType && canRemedy =>
          ;
        // Matching on types like case _: AnyRef { def bippy: Int } => doesn't work -- yet.
        case RefinedType(_, decls) if !decls.isEmpty =>
          getContext.unit.warning(tree.pos, s"a pattern match on a refinement type is unchecked")
        case _ =>
          log(checker.summaryString)
          if (checker.uninhabited) {
            val addendum = if (checker.unrelatable) "" else " (but due to erasure, the match may still succeed)"
            getContext.unit.warning(tree.pos, s"the intersection of $X0 and $P0 is uninhabited$addendum")
          }
          else if (checker.uncheckable) {
            val where = if (inPattern) "pattern " else ""
            val msg = (
              if (checker.uncheckableType =:= P) s"abstract type $where$P0"
              else s"${checker.uncheckableMessage} in type $where$P0"
            )
            getContext.unit.warning(tree.pos, s"$msg is unchecked since it is eliminated by erasure")
          }
      }
      true
    }
  }
}

private[typechecker] final class Checkability(val value: Int) extends AnyVal { }
private[typechecker] object Checkability {
  val StaticallyTrue    = new Checkability(0)
  val Uninhabited       = new Checkability(1)
  val RuntimeCheckable  = new Checkability(2)
  val Uncheckable       = new Checkability(3)
  val CheckabilityError = new Checkability(4)
}
