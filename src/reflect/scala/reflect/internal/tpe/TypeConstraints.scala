package scala
package reflect
package internal
package tpe

import scala.collection.{ generic }
import generic.Clearable


private[internal] trait TypeConstraints {
  self: SymbolTable =>
  import definitions._

  /** A log of type variable with their original constraints. Used in order
    *  to undo constraints in the case of isSubType/isSameType failure.
    */
  lazy val undoLog = newUndoLog

  protected def newUndoLog = new UndoLog

  class UndoLog extends Clearable {
    private type UndoPairs = List[(TypeVar, TypeConstraint)]
    //OPT this method is public so we can do `manual inlining`
    var log: UndoPairs = List()

    /*
     * These two methods provide explicit locking mechanism that is overridden in SynchronizedUndoLog.
     *
     * The idea behind explicit locking mechanism is that all public methods that access mutable state
     * will have to obtain the lock for their entire execution so both reads and writes can be kept in
     * right order. Originally, that was achieved by overriding those public methods in
     * `SynchronizedUndoLog` which was fine but expensive. The reason is that those public methods take
     * thunk as argument and if we keep them non-final there's no way to make them inlined so thunks
     * can go away.
     *
     * By using explicit locking we can achieve inlining.
     *
     * NOTE: They are made public for now so we can apply 'manual inlining' (copy&pasting into hot
     * places implementation of `undo` or `undoUnless`). This should be changed back to protected
     * once inliner is fixed.
     */
    def lock(): Unit = ()
    def unlock(): Unit = ()

    // register with the auto-clearing cache manager
    perRunCaches.recordCache(this)

    /** Undo all changes to constraints to type variables upto `limit`. */
    //OPT this method is public so we can do `manual inlining`
    def undoTo(limit: UndoPairs) {
      assertCorrectThread()
      while ((log ne limit) && log.nonEmpty) {
        val (tv, constr) = log.head
        tv.constr = constr
        log = log.tail
      }
    }

    /** No sync necessary, because record should only
      *  be called from within an undo or undoUnless block,
      *  which is already synchronized.
      */
    private[reflect] def record(tv: TypeVar) = {
      log ::= ((tv, tv.constr.cloneInternal))
    }

    def clear() {
      lock()
      try {
        if (settings.debug)
          self.log("Clearing " + log.size + " entries from the undoLog.")
        log = Nil
      } finally unlock()
    }

    // `block` should not affect constraints on typevars
    def undo[T](block: => T): T = {
      lock()
      try {
        val before = log

        try block
        finally undoTo(before)
      } finally unlock()
    }
  }

  /** @PP: Unable to see why these apparently constant types should need vals
    *  in every TypeConstraint, I lifted them out.
    */
  private lazy val numericLoBound = IntTpe
  private lazy val numericHiBound = intersectionType(List(ByteTpe, CharTpe), ScalaPackageClass)

  /** A class expressing upper and lower bounds constraints of type variables,
    * as well as their instantiations.
    */
  class TypeConstraint(lo0: List[Type], hi0: List[Type], numlo0: Type, numhi0: Type, avoidWidening0: Boolean = false) {
    def this(lo0: List[Type], hi0: List[Type]) = this(lo0, hi0, NoType, NoType)
    def this(bounds: TypeBounds) = this(List(bounds.lo), List(bounds.hi))
    def this() = this(List(), List())

    /*  Syncnote: Type constraints are assumed to be used from only one
     *  thread. They are not exposed in api.Types and are used only locally
     *  in operations that are exposed from types. Hence, no syncing of any
     *  variables should be ncessesary.
     */

    /** Guard these lists against AnyClass and NothingClass appearing,
      *  else loBounds.isEmpty will have different results for an empty
      *  constraint and one with Nothing as a lower bound.  [Actually
      *  guarding addLoBound/addHiBound somehow broke raw types so it
      *  only guards against being created with them.]
      */
    private var lobounds = lo0 filterNot typeIsNothing
    private var hibounds = hi0 filterNot typeIsAny
    private var numlo = numlo0
    private var numhi = numhi0
    private var avoidWidening = avoidWidening0

    def loBounds: List[Type] = if (numlo == NoType) lobounds else numlo :: lobounds
    def hiBounds: List[Type] = if (numhi == NoType) hibounds else numhi :: hibounds
    def avoidWiden: Boolean = avoidWidening

    def addLoBound(tp: Type, isNumericBound: Boolean = false) {
      // For some reason which is still a bit fuzzy, we must let Nothing through as
      // a lower bound despite the fact that Nothing is always a lower bound.  My current
      // supposition is that the side-effecting type constraint accumulation mechanism
      // depends on these subtype tests being performed to make forward progress when
      // there are mutally recursive type vars.
      // See pos/t6367 and pos/t6499 for the competing test cases.
      val mustConsider = tp.typeSymbol match {
        case NothingClass => true
        case _            => !(lobounds contains tp)
      }
      if (mustConsider) {
        if (isNumericBound && isNumericValueType(tp)) {
          if (numlo == NoType || isNumericSubType(numlo, tp))
            numlo = tp
          else if (!isNumericSubType(tp, numlo))
            numlo = numericLoBound
        }
        else lobounds ::= tp
      }
    }

    def checkWidening(tp: Type) {
      if(tp.isStable) avoidWidening = true
      else tp match {
        case HasTypeMember(_, _) => avoidWidening = true
        case _ =>
      }
    }

    def addHiBound(tp: Type, isNumericBound: Boolean = false) {
      // My current test case only demonstrates the need to let Nothing through as
      // a lower bound, but I suspect the situation is symmetrical.
      val mustConsider = tp.typeSymbol match {
        case AnyClass => true
        case _        => !(hibounds contains tp)
      }
      if (mustConsider) {
        checkWidening(tp)
        if (isNumericBound && isNumericValueType(tp)) {
          if (numhi == NoType || isNumericSubType(tp, numhi))
            numhi = tp
          else if (!isNumericSubType(numhi, tp))
            numhi = numericHiBound
        }
        else hibounds ::= tp
      }
    }

    def isWithinBounds(tp: Type): Boolean =
      lobounds.forall(_ <:< tp) &&
        hibounds.forall(tp <:< _) &&
        (numlo == NoType || (numlo weak_<:< tp)) &&
        (numhi == NoType || (tp weak_<:< numhi))

    private[this] var currentInst: Type = NoType // @M reduce visibility?
    def setInst(tp: Type): this.type = {
      tp match {
        case tv: TypeVar => devWarning(s"$this.setInst($tv) but that's a TypeVar!")
        case _           => currentInst = tv
      }
      this
    }
    def inst: Type = currentInst
    def instValid = inst match {
      case null | NoType => false
      case _: TypeVar    => false // happens during erroneous compilation at least; stack overflow
      case _             => true
    }
    def cloneInternal = (
      new TypeConstraint(lobounds, hibounds, numlo, numhi, avoidWidening)
        setInst inst
    )

    override def toString = {
      val boundsStr = {
        val lo    = loBounds filterNot typeIsNothing
        val hi    = hiBounds filterNot typeIsAny
        val lostr = if (lo.isEmpty) Nil else List(lo.mkString(" >: (", ", ", ")"))
        val histr = if (hi.isEmpty) Nil else List(hi.mkString(" <: (", ", ", ")"))

        lostr ++ histr mkString ("[", " | ", "]")
      }
      if (inst eq NoType) boundsStr
      else boundsStr + " _= " + inst.safeToString
    }
  }

  /** Solve constraint collected in types `tvars`.
    *
    *  @param tvars      All type variables to be instantiated.
    *  @param tparams    The type parameters corresponding to `tvars`
    *  @param variances  The variances of type parameters; need to reverse
    *                    solution direction for all contravariant variables.
    *  @param upper      When `true` search for max solution else min.
    */
  def solve(tvars: List[TypeVar], tparams: List[Symbol], variances: List[Variance], upper: Boolean): Boolean =
    solve(tvars, tparams, variances, upper, AnyDepth)


  def solveUpper(tvars: List[TypeVar], tparams: List[Symbol], variances: List[Variance]): Boolean = {
  }

  def solveLower(tvars: List[TypeVar], tparams: List[Symbol], variances: List[Variance]): Boolean = {
  }

  def solve(tvars: List[TypeVar], tparams: List[Symbol], variances: List[Variance], upper: Boolean, depth: Int): Boolean = {
    def instantiate(tp: Type) = logResult(s"$tp.instantiate($tparams, $tvars)")(tp.instantiateTypeParams(tparams, tvars))

    def solveOne(tvar: TypeVar, tparam: Symbol, v: Variance): Boolean = {
      val up =
    }

    def tparamBound(tparam: Symbol, variance: Variance): Type = (
      if (variance.isContravariant == !upper)
        tparam.info.bounds.hi.dealias
      else
        tparam.info.bounds.lo.dealias
    )

    def opposingBound(other: Symbol)

    def ignoreBound(tparam: Symbol) = tparamBound(tparam, variance) match {
      case `tparam`            => true
      case AnyClass if up      => true
      case NothingClass if !up => true
      case _                   => false
    }

    def instantiateUp(tvar: TypeVar, tparam: Symbol): Type = {
      def add(tp: Type) = tvar addHiBound logResult(s"$tvar addHiBound")(instantiate(tp))

      tparams foreach (other =>
        other.info.bounds.lo.dealias match {
          case tp @ TypeRef(_, `tparam`, _) => add(other.tpeHK)
          case _                            =>
        }
      )
    }
    def instantiateDown(tvar: TypeVar, tparam: Symbol): Type = {
      def add(tp: Type) = tvar addLoBound logResult(s"$tvar addLoBound")(instantiate(tp))

      tparams foreach (other =>
        other.info.bounds.hi.dealias match {
          case tp @ TypeRef(_, `tparam`, _) => add(other.tpeHK)
          case _                            =>
        }
      )
    }
    def glbOf(tvar: TypeVar) = depth match {
      case AnyDepth => glb(tvar.constr.hiBounds)
      case _        => glb(tvar.constr.hiBounds, depth)
    }
    def lubOf(tvar: TypeVar) = depth match {
      case AnyDepth => lub(tvar.constr.loBounds)
      case _        => lub(tvar.constr.loBounds, depth
    }

    for (tparam1 <- tparams ; tparam2 <- tparams ; if tparam1 ne tparam2) yield {
      val tpe1  = tparam1.tpeHK
      val bound = tparam2.info.bounds

    def cycleAmongParamsAndBounds(tparam: Symbol, tparam2: Symbol) = {
      def tpe1 = tparam.tpeHK
      val bound = if (up) tparam2.info.bounds.lo else tparam2.info.bounds.hi

      (    (bound contains tparam2)
        || (bound =:= tpe1)
      )
    }

      val cycleResults = map3(tvars, tparams, variances)((tvar2, tparam2, variance2) =>
           (tparam2 != tparam)
        && compareBoundToParmams(tparam, tparam2)
        && try tvar2.cycleCheck() finally solveOne(tvar2, tparam2, variance2)
      )
    }

    def solveOne(tvar: TypeVar, tparam: Symbol, v: Variance): Boolean = (
      if (tvar.instValid) true
      else if (cycleAmongParamsAndBounds(tvar, tparam)) false
      else if (v.isContravariant == upper) instantiateUp(tvar, tparam)
      else instantiateDown(tvar, tparam)
    )

    // Checking whether each type parameter fits within the bounds of
    // the type parameter under consideration.
    def isOneWithinBounds(tparam: Symbol, other: Symbol) = (
         (bound contains tparam)
      || (other.info.bounds.

    val solutions = map3(tvars, tparams, variances)(solveOne)
    val ok = tvars forall (_.boundsCheck())

    var caughtCycle = false
    def cycleGuard[T](tvar: TypeVar)(body: => T): T = {
      tvar.cycleMark()
      try body
      finally if (tvar2.cycleCheck()) caughtCycle = true
    }


    def solveOne(tvar: TypeVar, tparam: Symbol, variance: Variance) {
      cycleGuard {


      if (tvar.instValid) return
      val TypeBounds(lo, hi) = tparam.info.bounds
      val up                 = if (variance.isContravariant) !upper else upper
      tvar.cycleMark()

      val bound: Type = if (up) hi else lo

      //Console.println("solveOne0(tv, tp, v, b)="+(tvar, tparam, variance, bound))

      def compareBoundToParmams(tparam: Symbol, tparam2: Symbol) = {
        def tpe1 = tparam.tpeHK
        val TypeBounds(lo2, hi2) = tparam2.info.bounds

        (    (bound contains tparam2)
          || up && lo =:= tpe1
          || !up && hi =:= tpe1
        )
      }

      val cycleResults = map3(tvars, tparams, variances)((tvar2, tparam2, variance2) =>
           (tparam2 != tparam)
        && compareBoundToParmams(tparam, tparam2)
        && try tvar2.cycleCheck() finally solveOne(tvar2, tparam2, variance2)
      )
      val cyclic = (bound contains tparam) || (cycleResults exists (x => x))

      if (!cyclic) {
        if (up) {
          if (bound.typeSymbol != AnyClass) {
            log(s"$tvar addHiBound $bound.instantiateTypeParams($tparams, $tvars)")
            tvar addHiBound instantiate(bound)
          }
          for (tparam2 <- tparams)
            tparam2.info.bounds.lo.dealias match {
              case TypeRef(_, `tparam`, _) =>
                log(s"$tvar addHiBound $tparam2.tpeHK.instantiateTypeParams($tparams, $tvars)")
                tvar addHiBound instantiate(tparam2.tpeHK)
              case _ =>
            }
        } else {
          if (bound.typeSymbol != NothingClass && bound.typeSymbol != tparam) {
            log(s"$tvar addLoBound $bound.instantiateTypeParams($tparams, $tvars)")
            tvar addLoBound instantiate(bound)
          }
          for (tparam2 <- tparams)
            tparam2.info.bounds.hi.dealias match {
              case TypeRef(_, `tparam`, _) =>
                log(s"$tvar addLoBound $tparam2.tpeHK.instantiateTypeParams($tparams, $tvars)")
                tvar addLoBound instantiate(tparam2.tpeHK)
              case _ =>
            }
        }
      }
      tvar.clearInst() // necessary because hibounds/lobounds may contain tvar

      //println("solving "+tvar+" "+up+" "+(if (up) (tvar.constr.hiBounds) else tvar.constr.loBounds)+((if (up) (tvar.constr.hiBounds) else tvar.constr.loBounds) map (_.widen)))
      val newInst = (
        if (up) {
          if (depth != AnyDepth) glb(tvar.constr.hiBounds, depth) else glb(tvar.constr.hiBounds)
        } else {
          if (depth != AnyDepth) lub(tvar.constr.loBounds, depth) else lub(tvar.constr.loBounds)
        }
        )
      log(s"$tvar setInst $newInst")
      tvar setInst newInst
      //Console.println("solving "+tvar+" "+up+" "+(if (up) (tvar.constr.hiBounds) else tvar.constr.loBounds)+((if (up) (tvar.constr.hiBounds) else tvar.constr.loBounds) map (_.widen))+" = "+tvar.constr.inst)//@MDEBUG
    }

    // println("solving "+tvars+"/"+tparams+"/"+(tparams map (_.info)))
    foreach3(tvars, tparams, variances)(solveOne)
    tvars forall (tvar => tvar.constr.isWithinBounds(tvar.inst))
  }
}
