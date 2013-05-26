package scala
package reflect
package internal
package tpe

import scala.collection.{ mutable, immutable, generic }
import generic.Clearable

private[internal] trait TypeConstraints {
  self: SymbolTable =>
  import definitions._

  /** A log of type variable with their original constraints. Used in order
    *  to undo constraints in the case of isSubType/isSameType failure.
    */
  lazy val undoLog = newUndoLog

  protected def newUndoLog = new UndoLog

  val liveConstraints = mutable.Set[Int]()
  // val constraintsRolledBack = mutable.Set[Int]()
  val typeConstraintMap = mutable.Map[TypeConstraint, List[TypeConstraintState]]() withDefaultValue Nil
  def typeConstraintSync() {
    typeConstraintMap foreach { case (k, vs) =>
      typeConstraintMap(k) = vs filter (v => liveConstraints(v.id))
    }
  }
  var undoStarts = List[Int](-1)
  private def undoSpaces = "  " * (undoStarts.size - 1)
  def alreadyStacked = lastConstraintId == undoStarts.head
  def pushUndo(id: Int): Unit = {
    // undoStarts match {
    //   case -1 :: Nil             => ()
    //   case hd :: -1 :: Nil       => ()
    //   case x1 :: x2 :: -1 :: Nil =>
    //     ulog(x2 + "->")
    //     ulog(undoSpaces + id + "->")
    //   case _ =>
    //     ulog(undoSpaces + id + "->")
    // }
    undoStarts ::= id
  }
  def popUndo(id: Int): Unit = {
    assert(undoStarts.head == id, (undoStarts.head, id))
    undoStarts = undoStarts.tail
    // undoStarts match {
    //   case -1 :: Nil             => ()
    //   case hd :: -1 :: Nil       => ()
    //   case x1 :: x2 :: -1 :: Nil =>
    //     ulog(undoSpaces + "<-" + id)
    //     ulog("<-" + x2)
    //   case _ =>
    //     ulog(undoSpaces + "<-" + id)
    // }
    if (undoStarts.isEmpty) {
      ulog(s"Clearing ${liveConstraints.size} live constraints")
      liveConstraints.clear()
    }
    // ulog(undoSpaces + "<-" + id)
  }

  private val undoLogLog = sys.props contains "scalac.debug.undo"
  private def ulog(msg: => Any): Unit = {
    if (undoLogLog)
      Console.err.println(msg)
  }
  private var constraintId = 1
  def nextConstraintId() = {
    val result = try constraintId finally constraintId += 1
    liveConstraints += result
    result
  }
  def lastConstraintId = constraintId - 1
  def rollBackTo(id: Int) {
    liveConstraints --= (id until lastConstraintId)
    // val newRollbacks = id until lastConstraintId filter liveConstraints
    // if (newRollbacks.nonEmpty) {
    //   val roll_s = newRollbacks.toList match {
    //     case hd :: Nil => s"[$hd]"
    //     case elems     =>
    //       val min = elems.head.toInt
    //       val max = elems.last.toInt
    //       if (elems.size > max - min)
    //         s"[$min,$max]"
    //       else
    //         elems.mkString("[", ",", "]")
    //   }
    //   // Console.err.println(f"[rollback] $roll_s")
    //   // [$id,$lastConstraintId)")
    //   liveConstraints --= newRollbacks
    // }
    // for ((k, vs) <- typeConstraintMap) {
    //   val (keep, drop0) = vs partition (_.id <= id)
    //   val drop = drop0 //filter (_.instValid)
    //   typeConstraintMap(k) = keep
    //   if (drop.nonEmpty) {
    //     Console.err.println(s"Wound constraints for $k back to id=$id, dropped:")
    //     drop foreach (d => Console.err.println("  " + d))
    //   }
    //   // typeConstraintMap(k) = vs filter (c => (c.id <= id) || {
    //   //   if (c.instValid)
    //   //     Console.err.println(s"Dropping $c")

    //   //   false
    //   // })
    // }
  }

  def getConstraint(tc: TypeConstraint): TypeConstraintState = {
    typeConstraintMap(tc) match {
      case Nil                                 => TypeConstraint.InitialState
      case hd :: tl if !liveConstraints(hd.id) => typeConstraintMap(tc) = tl ; getConstraint(tc)
      case hd :: _                             => hd
    }
  }

  class UndoLog extends Clearable {
    def lock(): Unit = ()
    def unlock(): Unit = ()
    def clear() { typeConstraintMap.clear() }
    def dump() {
      val size    = typeConstraintMap.size
      val states  = typeConstraintMap.values.flatten.size
      val average = if (size == 0) 0 else states / size
      ulog(s"$size keys in map, which holds $states states (average: $average)")

      val pairs = typeConstraintMap.mapValues(_ filterNot (_.isEmpty)).toList filterNot (_._2.isEmpty) sortBy (-_._2.length)

      pairs foreach { case (tc, states) =>
        ulog(s"[states=${states.size}] $tc")
        states map (_.longString) filterNot (_ == "") foreach (s => ulog("    " + s))
        ulog("")
      }
    }
    // perRunCaches recordCache this
    // scala.sys addShutdownHook dump()

    @inline final def locked[T](body: => T): T = {
      lock()
      try body finally unlock()
    }

    @inline final def onStack[T](start: Int)(body: => T)(f: T => Unit): T = {
      pushUndo(start)
      val result = locked {
        try body finally popUndo(start)
      }
      if (start < lastConstraintId)
        f(result)

      result
    }

    @inline final def undoably[T](label: => String, body: => T)(undo: T => Boolean): T = {
      val start = lastConstraintId
      if (alreadyStacked)
        body
      else onStack(start)(body)(result =>
        if (undo(result))
          rollBackTo(start)
      )
    }

    // `block` should not affect constraints on typevars
    @inline final def undo[T](label: => String, body: => T): T = undoably(label, body)(_ => true)
  }

  /** @PP: Unable to see why these apparently constant types should need vals
    *  in every TypeConstraint, I lifted them out.
    */
  private lazy val numericLoBound = IntTpe
  private lazy val numericHiBound = intersectionType(List(ByteTpe, CharTpe), ScalaPackageClass)

  case class TypeConstraintState (
    prev: TypeConstraintState,
    origin: Symbol         = NoSymbol,
    lo: List[Type]         = Nil,
    hi: List[Type]         = Nil,
    numlo: Type            = NoType,
    numhi: Type            = NoType,
    avoidWidening: Boolean = false,
    inst: Type             = NoType
  ) {
    def copy(
      origin: Symbol         = this.origin,
      lo: List[Type]         = this.lo,
      hi: List[Type]         = this.hi,
      numlo: Type            = this.numlo,
      numhi: Type            = this.numhi,
      avoidWidening: Boolean = this.avoidWidening,
      inst: Type             = this.inst
    ): TypeConstraintState = new TypeConstraintState(prev = this, origin, lo, hi, numlo, numhi, avoidWidening, inst)

    val id = nextConstraintId()
    def instValid = (inst ne null) && (inst ne NoType)
    def loBounds: List[Type] = if (numlo == NoType) lo else numlo :: lo
    def hiBounds: List[Type] = if (numhi == NoType) hi else numhi :: hi
    def isEmpty = (
         (loBounds filterNot typeIsNothing).isEmpty
      && (hiBounds filterNot typeIsAny).isEmpty
    )
    def cloneState() = new TypeConstraintState(prev = TypeConstraint.InitialState, origin, lo, hi, numlo, numhi, avoidWidening, inst)
    private def origin_s = origin match {
      case NoSymbol                              => "<>"
      case sym if sym.safeOwner == sym.enclClass => s"${sym.safeOwner.nameString}#${sym.nameString}"
      case sym                                   => s"${sym.enclClass.nameString}.${sym.safeOwner.nameString}#${sym.nameString}"
    }
    override def toString = f"$origin_s  $inst"
    def prevLongString: String = prev match {
      case null                    => ""
      case prev if !prev.instValid => prev.prevLongString
      case _                       => prev.longString.lines.mkString(s" { // id=$id\n    ", "\n    ", "\n}")
    }
    def longString: String = toList filter (_.instValid) mkString " -> " //s"$this$prevLongString"
    def toList: List[TypeConstraintState] = this :: (prev match {
      case null => Nil
      case _    => prev.toList
    })
  }

  object TypeConstraint {
    val InitialState = new TypeConstraintState(prev = null)

    def apply(): TypeConstraint                                   = apply(NoSymbol)
    def apply(tparam: Symbol): TypeConstraint                     = apply(InitialState.copy(origin = tparam))
    def apply(tparam: Symbol, bounds: TypeBounds): TypeConstraint = apply(InitialState.copy(origin = tparam, lo = bounds.lo :: Nil, hi = bounds.hi :: Nil))
    def apply(bounds: TypeBounds): TypeConstraint                 = apply(InitialState.copy(lo = bounds.lo :: Nil, hi = bounds.hi :: Nil))
    def apply(lo0: List[Type], hi0: List[Type]): TypeConstraint   = apply(InitialState.copy(lo = lo0, hi = hi0))
    def apply(state: TypeConstraintState): TypeConstraint         = new TypeConstraint newState state
  }

  /** A class expressing upper and lower bounds constraints of type variables,
    * as well as their instantiations.
    */
  class TypeConstraint private () {
    def state         = getConstraint(this)
    def tparam        = state.origin
    def origin        = state.origin
    def lobounds      = state.lo
    def hibounds      = state.hi
    def avoidWidening = state.avoidWidening
    def inst          = state.inst
    def numlo         = state.numlo
    def numhi         = state.numhi
    def loBounds      = state.loBounds
    def hiBounds      = state.hiBounds

    def newState(state: TypeConstraintState): this.type = {
      typeConstraintMap(this) ::= state
      this
    }
    def numlo_=(tp: Type) = newState(state.copy(numlo = tp))
    def numhi_=(tp: Type) = newState(state.copy(numhi = tp))
    def lobounds_=(tps: List[Type]) = newState(state.copy(lo = tps))
    def hibounds_=(tps: List[Type]) = newState(state.copy(hi = tps))
    def avoidWidening_=(value: Boolean) = newState(state.copy(avoidWidening = value))
    def inst_=(tp: Type) = newState(state.copy(inst = tp))

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
    // private var lobounds = lo0 filterNot typeIsNothing
    // private var hibounds = hi0 filterNot typeIsAny
    // private var numlo = numlo0
    // private var numhi = numhi0
    // private var avoidWidening = avoidWidening0
    // def loBounds: List[Type] = if (numlo == NoType) lobounds else numlo :: lobounds
    // def hiBounds: List[Type] = if (numhi == NoType) hibounds else numhi :: hibounds
    // def avoidWiden: Boolean = state.avoidWidening

    def addLoBound(tp: Type, isNumericBound: Boolean = false) {
      // For some reason which is still a bit fuzzy, we must let Nothing through as
      // a lower bound despite the fact that Nothing is always a lower bound.  My current
      // supposition is that the side-effecting type constraint accumulation mechanism
      // depends on these subtype tests being performed to make forward progress when
      // there are mutally recursive type vars.
      // See pos/t6367 and pos/t6499 for the competing test cases.
      val mustConsider = tp.typeSymbol match {
        case NothingClass => true
        case _            => !(loBounds contains tp)
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


    // var inst: Type = NoType // @M reduce visibility?
    def instValid = inst ne NoType
    def cloneInternal = TypeConstraint(state.cloneState())

    override def toString = {
      val boundsStr = {
        val lo    = loBounds filterNot typeIsNothing
        val hi    = hiBounds filterNot typeIsAny
        val lostr = if (lo.isEmpty) Nil else List(lo.mkString(" >: (", ", ", ")"))
        val histr = if (hi.isEmpty) Nil else List(hi.mkString(" <: (", ", ", ")"))

        lostr ++ histr match {
          case Nil => "<empty>"
          case xs  => xs mkString ("[", " | ", "]")
        }
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
  def solve(tvars: List[TypeVar], tparams: List[Symbol],
            variances: List[Variance], upper: Boolean): Boolean =
    solve(tvars, tparams, variances, upper, AnyDepth)

  def solve(tvars: List[TypeVar], tparams: List[Symbol],
            variances: List[Variance], upper: Boolean, depth: Int): Boolean = {

    def solveOne(tvar: TypeVar, tparam: Symbol, variance: Variance) {
      if (tvar.constr.inst == NoType) {
        val up = if (variance.isContravariant) !upper else upper
        tvar.constr.inst = null
        val bound: Type = if (up) tparam.info.bounds.hi else tparam.info.bounds.lo
        //Console.println("solveOne0(tv, tp, v, b)="+(tvar, tparam, variance, bound))
        var cyclic = bound contains tparam
        foreach3(tvars, tparams, variances)((tvar2, tparam2, variance2) => {
          val ok = (tparam2 != tparam) && (
            (bound contains tparam2)
              ||  up && (tparam2.info.bounds.lo =:= tparam.tpeHK)
              || !up && (tparam2.info.bounds.hi =:= tparam.tpeHK)
            )
          if (ok) {
            if (tvar2.constr.inst eq null) cyclic = true
            solveOne(tvar2, tparam2, variance2)
          }
        })
        if (!cyclic) {
          if (up) {
            if (bound.typeSymbol != AnyClass) {
              log(s"$tvar addHiBound $bound.instantiateTypeParams($tparams, $tvars)")
              tvar addHiBound bound.instantiateTypeParams(tparams, tvars)
            }
            for (tparam2 <- tparams)
              tparam2.info.bounds.lo.dealias match {
                case TypeRef(_, `tparam`, _) =>
                  log(s"$tvar addHiBound $tparam2.tpeHK.instantiateTypeParams($tparams, $tvars)")
                  tvar addHiBound tparam2.tpeHK.instantiateTypeParams(tparams, tvars)
                case _ =>
              }
          } else {
            if (bound.typeSymbol != NothingClass && bound.typeSymbol != tparam) {
              log(s"$tvar addLoBound $bound.instantiateTypeParams($tparams, $tvars)")
              tvar addLoBound bound.instantiateTypeParams(tparams, tvars)
            }
            for (tparam2 <- tparams)
              tparam2.info.bounds.hi.dealias match {
                case TypeRef(_, `tparam`, _) =>
                  log(s"$tvar addLoBound $tparam2.tpeHK.instantiateTypeParams($tparams, $tvars)")
                  tvar addLoBound tparam2.tpeHK.instantiateTypeParams(tparams, tvars)
                case _ =>
              }
          }
        }
        tvar.constr.inst = NoType // necessary because hibounds/lobounds may contain tvar

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
    }

    // println("solving "+tvars+"/"+tparams+"/"+(tparams map (_.info)))
    foreach3(tvars, tparams, variances)(solveOne)
    tvars forall (tvar => tvar.constr.isWithinBounds(tvar.constr.inst))
  }
}
