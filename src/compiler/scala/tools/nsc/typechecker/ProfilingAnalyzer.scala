/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package typechecker

import scala.collection.{ mutable, immutable }
import util.{ SourceFile, TreeTracer }
import util.Position._
import TreeTracer._

object ProfilingAnalyzer {
  private var tracers: List[TreeTracer] = Nil

  def showAtShutdown(t: TreeTracer) {
    tracers ::= t
  }
  scala.sys addShutdownHook {
    tracers.reverse foreach { t =>
      println(t)
      t.show
    }
  }
}

trait ProfilingAnalyzer extends Analyzer { prof =>
  import global._

  override def newNamer(context: Context): Namer = new ProfilingNamer(context)
  override def newTyper(context: Context): Typer = new ProfilingTyper(context)

  override def finishTyper(): Unit = {
    super.finishTyper()
    tracer.show()
  }

  class ProfilingNamer(context0: Context) extends Namer(context0) {
    // def enterSym(tree: Tree): Context
  }

  class ProfilingTyper(context0: Context) extends Typer(context0) with TypedTrees {
    override def typed(tree: Tree, mode: Int, pt: Type): Tree =
      typing(tree, hasPt = pt != WildcardType)(super.typed(tree, mode, pt))
    override def typedApply(tree: Apply, mode: Int, pt: Type): Tree =
      typing(tree, hasPt = pt != WildcardType)(super.typedApply(tree, mode, pt))
    override def typedBlock(block: Block, mode: Int, pt: Type): Block =
      typing(block, hasPt = pt != WildcardType)(super.typedBlock(block, mode, pt))
    override def typedIdent(tree: Ident, mode: Int, pt: Type): Tree =
      typing(tree, hasPt = pt != WildcardType)(super.typedIdent(tree, mode, pt))
    override def typedStat(stat: Tree, exprOwner: Symbol): Tree =
      typing(stat)(super.typedStat(stat, exprOwner))
    override def typedClassDef(cdef: ClassDef): ClassDef =
      typing(cdef)(super.typedClassDef(cdef))
    override def typedModuleDef(mdef: ModuleDef): ModuleDef =
      typing(mdef)(super.typedModuleDef(mdef))
    override def typedDefDef(ddef: DefDef): DefDef =
      typing(ddef)(super.typedDefDef(ddef))
    override def typedTypeDef(tdef: TypeDef): TypeDef =
      typing(tdef)(super.typedTypeDef(tdef))
    override def typedValDef(vdef: ValDef): ValDef =
      typing(vdef)(super.typedValDef(vdef))
    override def typedTemplate(templ: Template, parents1: List[Tree]): Template =
      typing(templ)(super.typedTemplate(templ, parents1))
  }

  private var isCounting = false
  // private var ptStack: List[Type]
  private var counts, skipped, ptNanos, wildNanos = 0L
  scala.sys addShutdownHook {
    println(s"Counted $counts typed calls, skipped $skipped.")
    println(f"  pt secs: ${ptNanos / 1e9 }%.3f")
    println(f"wild secs: ${wildNanos / 1e9 }%.3f")
  }
  private def timing[T](body: => T): (T, Long) = {
    counts += 1
    isCounting = true
    try {
      val startTime  = System.nanoTime
      val result     = body
      val endTime    = System.nanoTime

      (result, endTime - startTime)
    }
    finally isCounting = false
  }

  private val openTrees = mutable.HashSet[Tree]()
  private def typing[T](tree: Tree, hasPt: Boolean)(body: => T): T = {
    // ptStack ::= ((pt, System.nanoTime)
    // try
    // finally ptStack = ptStack.tail

    if (isCounting) {
      skipped += 1
      typing(tree)(body)
    }
    else {
      val (res, elapsed) = timing(typing(tree)(body))
      try res finally if (hasPt) ptNanos += elapsed else wildNanos += elapsed
    }
  }
  protected def typing[T](tree: Tree)(body: => T): T = {
    // No recursion for now
    if (openTrees(tree)) body
    else {
      openTrees += tree
      try tracer.enter[T](tree)(body)
      finally openTrees -= tree
    }
  }

  protected object tracer extends TreeTracer {
    type ReceiverType = Tree
    //
    // locally {
    //   ProfilingAnalyzer showAtShutdown this
    // }

    val minimumMillis = settings.typerTimings.value
    val shownLines = mutable.HashSet[(SourceFile, Int)]()

    object treeConsolidator extends ConsolidatingTree[TNode] {
      def newChildren(node: TNode, kids: List[TNode]) = node.copy(children = kids)
      def survives(node: TNode)                       = node.totalMillis >= minimumMillis
    }
    override def rootString(root: Tree)   = (
      try "\nIn file " + root.pos.source.file
      catch { case _: UnsupportedOperationException => "???" }
    )
    override def receiverPos(recv: Tree)  = s"(line ${recv.pos.safeLine})"
    override def consolidate(root: TNode) = treeConsolidator consolidate root
    override def consolidationString      = " to typer time >= " + minimumMillis + " ms"

    def symString(sym: Symbol)   = atPhase(currentRun.typerPhase)(sym.defString)
    def posString(pos: Position) = "%s:%s".format(pos.source.file.name, pos.line)
    def lineString(t: Tree) = {
      val s = t.pos.lineContent.trim
      if (s.length > 30) s.substring(0, 27) + "..." else s
    }

    def kindString(t: Tree) = t match {
      case x: DefDef                        => "def"
      case x: ValDef if x.mods.isMutable    => "var"
      case x: ValDef                        => "val"
      case x: ClassDef                      => "class"
      case x: ModuleDef                     => "object"
      case x: TypeDef                       => "type"
      case x: PackageDef                    => "package"
      case _                                => t.summaryString
    }

    def simpleString(t: Tree) = t match {
      case x: DefTree => kindString(x) + " " + x.name.decode
      case x: RefTree => kindString(x) + " " + x.name.decode
      case x          => x.summaryString
    }

    def treeString(t: Tree) = {
      val pos = normalizePosition(t.pos)
      if (!pos.isDefined) simpleString(t)
      else {
        val pair = ((pos.source, pos.line))

        if (!shownLines(pair)) {
          shownLines += pair
          val s = pos.lineContent.trim
          if (s.length > 30) s.substring(0, 27) + "..." else s
        }
        else t match {
          case x: DefTree => kindString(x) + " " + x.name.decode
          case x: RefTree => kindString(x) + " " + x.name.decode
          case x          => x.summaryString
        }
      }
    }
    override def stringify(value: Any): String = value match {
      case t: Tree                          => treeString(t)
      case xs: Traversable[_] if xs.isEmpty => "Nil"
      case _                                => super.stringify(value)
    }
    override def toString = "TreeTracer " + getClass.getName.split("\\.").last + " # " + System.identityHashCode(this)
  }
}
