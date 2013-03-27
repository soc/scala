/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Iulian Dragos
 */

package scala.tools.nsc
package transform

import symtab.Flags
import Flags.SYNTHETIC

/** Perform tail recursive call elimination.
 *
 *  @author Iulian Dragos
 *  @version 1.0
 */
abstract class TailCalls extends Transform {
  import global._                     // the global environment
  import definitions._                // standard classes and methods
  import typer.typedPos               // methods to type trees
  import treeInfo.{ hasSynthCaseSymbol, Applied }

  val phaseName: String = "tailcalls"

  def newTransformer(unit: CompilationUnit): Transformer =
    new TailCallElimination(unit)

  /** Create a new phase which applies transformer */
  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new Phase(prev)

  /** The phase defined by this transform */
  class Phase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
    def apply(unit: global.CompilationUnit) {
      if (!(settings.debuginfo.value == "notailcalls")) {
        newTransformer(unit).transformUnit(unit)
      }
    }
  }

  /**
   * A Tail Call Transformer
   *
   * @author     Erik Stenman, Iulian Dragos
   * @version    1.1
   *
   * What it does:
   * <p>
   *   Finds method calls in tail-position and replaces them with jumps.
   *   A call is in a tail-position if it is the last instruction to be
   *   executed in the body of a method.  This is done by recursing over
   *   the trees that may contain calls in tail-position (trees that can't
   *   contain such calls are not transformed). However, they are not that
   *   many.
   * </p>
   * <p>
   *   Self-recursive calls in tail-position are replaced by jumps to a
   *   label at the beginning of the method. As the JVM provides no way to
   *   jump from a method to another one, non-recursive calls in
   *   tail-position are not optimized.
   * </p>
   * <p>
   *   A method call is self-recursive if it calls the current method and
   *   the method is final (otherwise, it could
   *   be a call to an overridden method in a subclass). Furthermore, If
   *   the method has type parameters, the call must contain these
   *   parameters as type arguments. Recursive calls on a different instance
   *   are optimized. Since 'this' is not a local variable, a dummy local val
   *   is added and used as a label parameter. The backend knows to load
   *   the corresponding argument in the 'this' (local at index 0). This dummy local
   *   is never used and should be cleand up by dead code elimination (when enabled).
   * </p>
   * <p>
   *   If a method contains self-recursive calls, a label is added to at
   *   the beginning of its body and the calls are replaced by jumps to
   *   that label.
   * </p>
   * <p>
   *   Assumes: `Uncurry` has been run already, and no multiple
   *            parameter lists exit.
   * </p>
   */
  class TailCallElimination(unit: CompilationUnit) extends Transformer {
    private def defaultReason = "it contains a recursive call not in tail position"
    private val failPositions = perRunCaches.newMap[TailContext, Position]()
    private val failReasons   = perRunCaches.newMap[TailContext, String]()
    private def tailrecFailure(pos: Position, method: Symbol, reason: String) {
      unit.error(pos, s"could not optimize @tailrec annotated $method: $reason")
    }
    private def tailrecFailure(ctx: TailContext) {
      val method           = ctx.method
      val failReason       = failReasons.getOrElse(ctx, defaultReason)
      val failPos          = failPositions.getOrElse(ctx, ctx.methodPos)
      tailrecFailure(failPos, method, failReason)
    }

    private var ctx: TailContext = EmptyTailContext
    @inline private def settingContext[T](newContext: TailContext)(op: => T): T = {
      val saved = ctx
      ctx = newContext
      try op finally ctx = saved
    }
    @inline private def yesTail[T](op: => T): T = settingContext(new ClonedTailContext(ctx, tailPos = true))(op)
    @inline private def noTail[T](op: => T): T  = settingContext(new ClonedTailContext(ctx, tailPos = false))(op)

    private def isBooleanOp(fun: Tree) = fun.symbol == Boolean_or || fun.symbol == Boolean_and

    /** Has the label been accessed? Then its symbol is in this set. */
    private val accessed = perRunCaches.newSet[Symbol]()
    // `accessed` was stored as boolean in the current context -- this is no longer tenable
    // with jumps to labels in tailpositions now considered in tailposition,
    // a downstream context may access the label, and the upstream one will be none the wiser
    // this is necessary because tail-calls may occur in places where syntactically they seem impossible
    // (since we now consider jumps to labels that are in tailposition, such as matchEnd(x) {x})

    sealed trait TailContext {
      def method: Symbol          // current method
      def tparams: List[Symbol]   // type parameters
      def methodPos: Position     // default position for failure reporting
      def tailPos: Boolean        // context is in tail position
      def label: Symbol           // new label, tail call target
      def tailLabels: Set[Symbol]

      def currentClass                    = method.enclClass
      def enclosingType                   = currentClass.typeOfThis
      def isEligible                      = method.isEffectivelyFinal
      def isTailLabel(fun: Tree)          = fun.symbol.isLabel && tailLabels(fun.symbol)
      def rewriteApply(tree: Apply): Tree = noTail(TailCallElimination.super.transform(tree))
      def checkApply(tree: Tree): Unit    = ()

      override def toString = s"${method.name} tparams=$tparams tailPos=$tailPos label=$label label info=${label.info}"
    }

    object EmptyTailContext extends TailContext {
      def method     = NoSymbol
      def tparams    = Nil
      def methodPos  = NoPosition
      def tailPos    = false
      def label      = NoSymbol
      def tailLabels = Set.empty[Symbol]
    }

    class DefDefTailContext(dd: DefDef) extends TailContext {
      def method    = dd.symbol
      def tparams   = dd.tparams map (_.symbol)
      def methodPos = dd.pos
      def vparamss  = dd.vparamss
      def tailPos   = true
      def mandatory = method hasAnnotation TailrecClass

      lazy val label      = mkLabel()
      // labels are local to a method, so only traverse the rhs of a defdef
      lazy val tailLabels = new TailPosLabelsTraverser collect dd.rhs

      private def thisClass = currentClass.typeOfThis

      /* A possibly polymorphic apply to be considered for tail call transformation. */
      override def rewriteApply(tree: Apply): Tree = {
        val Applied(fun, targs, args :: Nil) = tree
        val receiver: Tree = fun match {
          case Select(qual, _)  => qual
          case _                => typer typed This(currentClass)
        }
        def receiverIsSame  = enclosingType.widen =:= receiver.tpe.widen
        def receiverIsSuper = /*!receiverIsSame &&*/ (enclosingType.widen <:< receiver.tpe.widen)
        def isRecursiveCall = (method eq fun.symbol) && tailPos
        def matchesTypeArgs = tparams sameElements targs.map(_.tpe.typeSymbol)

        // def pass(failMessage: String, failPos: Position = fun.pos): Tree = {
        //   failReasons(this)   = failMessage
        //   failPositions(this) = failPos
        //   super.rewriteApply(tree)
        // }
        def rewrite(): Tree = {
          debuglog("Rewriting tail recursive call:  " + fun.pos.lineContent.trim)
          accessed += label
          noTail(typedPos(fun.pos)(Apply(Ident(label), transformTrees(receiver :: args))))
        }
        // this is to detect tailcalls in translated matches
        // it's a one-argument call to a label that is in a tailposition and that looks like label(x) {x}
        // thus, the argument to the call is in tailposition
        if (isTailLabel(fun)) args match {
          case arg :: Nil =>
            debuglog(s"in tailpos label: $arg")
            val arg1 = yesTail(transform(arg))
            // we tail-called -- TODO: shield from false-positives where we rewrite but don't tail-call
            // must leave the jump to the original tailpos-label (fun)!
            // there might be *a* tailcall *in* res, but it doesn't mean res *always* tailcalls
            if (arg ne arg1)
              return treeCopy.Apply(tree, fun, arg1 :: Nil)
          case _ =>
        }

        val rewritable = (
             tailPos
          && (method eq fun.symbol)
          && receiverIsSame
          && matchesTypeArgs
        )

        if (isBooleanOp(fun))
          treeCopy.Apply(tree, fun, transformTrees(args))
        else if (rewritable)
          rewrite()
        else {
          if (mandatory)
            tailrecError(tree)

          super.rewriteApply(tree)
        }
      }

      lazy val transformedRhs: Tree = {
        val rhs = settingContext(this)(transform(dd.rhs))
        if (!accessed(label)) rhs else {
          val thisParam = method.newValue(nme.THIS, methodPos, SYNTHETIC) setInfo thisClass
          val params    = vparamss.flatten map (_.symbol)
          log(s"Performed tailcall transformation on ${method.fullLocationString}")

          typedPos(methodPos) {
            Block(
              ValDef(thisParam, This(currentClass)) :: Nil,
              LabelDef(label, thisParam :: params, rhs)
            )
          }
        }
      }
      private def mkLabel() = {
        val label = method.newLabel(newTermName("_" + method.name), method.pos)
        val thiz  = method.newSyntheticValueParam(thisClass)
        val info  = MethodType(thiz :: method.tpe.params, method.tpe.finalResultType)

        label setInfo info.substSym(method.tpe.typeParams, tparams)
      }

      def tailrecError(tree: Apply) {
        val Applied(fun, targs, args :: Nil) = tree
        val receiver = fun match {
          case Select(qual, _) => qual
          case _               => EmptyTree
        }
        def isTail          = (method eq fun.symbol) && tailPos
        def receiverIsSame  = (receiver eq EmptyTree) || (enclosingType.widen =:= receiver.tpe.widen)
        def receiverIsSuper = !receiverIsSame && (enclosingType.widen <:< receiver.tpe.widen)
        def matchesTypeArgs = tparams sameElements targs.map(_.tpe.typeSymbol)
        val message         = (
          if (!isTail)
            "it contains a recursive call outside of tail position"
          else if (receiverIsSuper)
            "it contains a recursive call targeting supertype " + fun.tpe
          else if (!matchesTypeArgs)
            "it contains a recursive call with different type arguments"
          else if (!receiverIsSame)
            "it changes type of 'this' on a polymorphic recursive call"
          else
            s"unknown reason: tailPos=$tailPos tree=$tree fun=$fun receiver=$receiver"
        )
        tailrecFailure(tree.pos, method, message)
      }

      def isRecursiveCall(sym: Symbol): Boolean = (
           (sym.overrideChain contains method)
        || (method.overrideChain contains sym)
      )
      def recursiveCallsIn(t: Tree): List[Apply] = t collect { case t @ Apply(fn, _) if isRecursiveCall(fn.symbol) => t }

      override def checkApply(tree: Tree): Unit = recursiveCallsIn(tree) foreach tailrecError

      def checkDefDef() = {
        if (!isEligible)
          tailrecFailure(dd.pos, method, "it is neither private nor final so can be overridden")
        else if (recursiveCallsIn(dd.rhs).isEmpty)
          tailrecFailure(dd.pos, method, "it contains no recursive calls")
      }
    }

    class ClonedTailContext(that: TailContext, override val tailPos: Boolean) extends TailContext {
      def method     = that.method
      def tparams    = that.tparams
      def methodPos  = that.methodPos
      def tailLabels = that.tailLabels
      def label      = that.label
    }

    override def transform(tree: Tree): Tree = {
      def sym         = tree.symbol

      tree match {
        case ValDef(_, _, _, _) if sym.isLazy && (sym hasAnnotation TailrecClass) =>
          unit.error(tree.pos, "lazy vals are not tailcall transformed")
          super.transform(tree)

        case dd @ DefDef(_, _, _, _, _, rhs0) if !dd.symbol.hasAccessorFlag =>
          assert(dd.symbol.enclClass == currentClass, ((dd.symbol.enclClass, currentClass)))
          val newCtx = new DefDefTailContext(dd)
          if (newCtx.mandatory)
            newCtx.checkDefDef()

          deriveDefDef(tree)(_ => newCtx.transformedRhs)

        // a translated match
        case Block(stats, expr) if stats forall hasSynthCaseSymbol =>
          // the assumption is once we encounter a case, the remainder of the block will consist of cases
          // the prologue may be empty, usually it is the valdef that stores the scrut
          val (prologue, cases) = stats span (s => !s.isInstanceOf[LabelDef])
          treeCopy.Block(tree,
            noTail(transformTrees(prologue)) ++ transformTrees(cases),
            transform(expr)
          )

        // a translated casedef
        case LabelDef(_, _, body) if hasSynthCaseSymbol(tree) =>
          deriveLabelDef(tree)(transform)

        case Block(stats, expr) =>
          treeCopy.Block(tree,
            noTail(transformTrees(stats)),
            transform(expr)
          )

        case CaseDef(pat, guard, body) =>
          deriveCaseDef(tree)(transform)

        case If(cond, thenp, elsep) =>
          treeCopy.If(tree,
            cond,
            transform(thenp),
            transform(elsep)
          )

        case Match(selector, cases) =>
          treeCopy.Match(tree,
            noTail(transform(selector)),
            transformTrees(cases).asInstanceOf[List[CaseDef]]
          )

        case Try(block, catches, finalizer @ EmptyTree) =>
          // SI-1672 Catches are in tail position when there is no finalizer
          treeCopy.Try(tree,
            noTail(transform(block)),
            transformTrees(catches).asInstanceOf[List[CaseDef]],
            EmptyTree
          )

        case Try(block, catches, finalizer) =>
          // no calls inside a try are in tail position if there is a finalizer, but keep recursing for nested functions
          noTail(
            treeCopy.Try(tree,
              transform(block),
              transformTrees(catches).asInstanceOf[List[CaseDef]],
              transform(finalizer)
            )
          )
        case app @ Apply(fun, _) if ctx.isEligible && !isBooleanOp(fun) =>
          ctx rewriteApply app
        case Select(qual, name) =>
          treeCopy.Select(tree, noTail(transform(qual)), name)
        case Alternative(_) | Star(_) | Bind(_, _) =>
          abort("We should've never gotten inside a pattern")
        case EmptyTree | Super(_, _) | This(_) | Ident(_) | Literal(_) | Function(_, _) | TypeTree() =>
          tree
        case _ =>
          super.transform(tree)
      }
    }
  }

  // collect the LabelDefs (generated by the pattern matcher) in a DefDef that are in tail position
  // the labels all look like: matchEnd(x) {x}
  // then, in a forward jump `matchEnd(expr)`, `expr` is considered in tail position (and the matchEnd jump is replaced by the jump generated by expr)
  class TailPosLabelsTraverser extends Traverser {
    private val tailLabels = perRunCaches.newSet[Symbol]()
    private var maybeTail  = true // since we start in the rhs of a DefDef

    @inline private def settingMaybeTail[T](value: Boolean)(op: => T): T = {
      val saved = maybeTail
      maybeTail = value
      try op finally maybeTail = saved
    }
    @inline private def yesTail[T](op: => T): T = settingMaybeTail(true)(op)
    @inline private def noTail[T](op: => T): T  = settingMaybeTail(false)(op)

    def collect(tree: Tree): Set[Symbol] = {
      traverse(tree)
      tailLabels.toSet
    }

    override def traverse(tree: Tree) = tree match {
      // we're looking for label(x){x} in tail position, since that means `a` is in tail position in a call `label(a)`
      case LabelDef(_, arg :: Nil, body@Ident(_)) if arg.symbol == body.symbol =>
        if (maybeTail) tailLabels += tree.symbol

      case LabelDef(_, _, body) if hasSynthCaseSymbol(tree) =>   // a translated casedef
        traverse(body)

      // jumps to matchEnd are transparent; need this case for nested matches
      // (and the translated match case below does things in reverse for this case's sake)
      case Apply(fun, arg :: Nil) if hasSynthCaseSymbol(fun) && tailLabels(fun.symbol) =>
        traverse(arg)

      case Apply(fun, args) if fun.symbol == Boolean_or || fun.symbol == Boolean_and =>
        traverseTrees(args)

      // a translated match
      case Block(stats, expr) if stats forall hasSynthCaseSymbol =>
        // the assumption is once we encounter a case, the remainder of the block will consist of cases
        // the prologue may be empty, usually it is the valdef that stores the scrut
        val (prologue, cases) = stats span (s => !s.isInstanceOf[LabelDef])
        traverse(expr)
        traverseTrees(cases.reverse)      // reverse so that we enter the matchEnd LabelDef before we see jumps to it
        noTail(traverseTrees(prologue))   // selector (may be absent)

      case CaseDef(pat, guard, body) =>
        traverse(body)

      case Match(selector, cases) =>
        noTail(traverse(selector))
        traverseTrees(cases)

      case Block(stats, expr) =>
        noTail(traverseTrees(stats))
        traverse(expr)

      case If(cond, thenp, elsep) =>
        traverse(thenp)
        traverse(elsep)

      case Try(block, catches, finalizer) =>
        noTail(super.traverse(tree))

      case DefDef(_, _, _, _, _, _)                                       => // we are run per-method
      case Apply(_, _) | EmptyTree | Super(_, _) | This(_) | Select(_, _) =>
      case Ident(_) | Literal(_) | Function(_, _) | TypeTree()            =>
      case _                                                              => super.traverse(tree)
    }
  }
}
