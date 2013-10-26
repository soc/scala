package scala
package tools
package nsc

import ast.{ SourceTokens, TreeCoverage }
import SourceTokens._
import scala.reflect.internal.util._
import scala.reflect.internal.Chars._
import scala.tools.nsc.ast.parser.Tokens._
import scala.collection.{ mutable, immutable }
import scala.annotation.{ switch, tailrec }
import scala.io.AnsiColor._

case class CString(str: String)(color: String) {
  // def + (that: CStr
  def length = str.length
  override def toString = if (color == "") str else inColor(color)(str)
}

final class IntRange private (val bits: Long) extends AnyVal {
  def start: Int       = (bits >>> 32).toInt
  def end: Int         = (bits & 0xFFFFFFFF).toInt
  def indices          = start to end
  def apply(x: Int)    = this contains x
  def contains(x: Int) = indices contains x
  def toVector         = indices.toVector
  def length           = end - start

  override def toString = s"[$start,$end)"
}
object IntRange {
  def apply(start: Int, end: Int): IntRange = {
    require(start <= end, s"$start > $end")
    new IntRange((start.toLong << 32) | end.toLong)
  }
}
object IntRanges {
  def apply(elems: Iterable[Int]): Vector[IntRange] = (
    if (elems.isEmpty) Vector() else {
      val start = elems.head
      val len   = elems.iterator.zipWithIndex count { case (x, i) => x == start + i }
      IntRange(start, start + len) +: apply(elems drop len)
    }
  )
}

trait Node[T <: Node[_]] {
  def range: IntRange
  def children: Seq[T]
  def hasChild(p: T => Boolean) = children exists p

  def covers(index: Int)                  = range contains index
  def owns(index: Int)                    = covers(index) && !hasChild(_ covers index)
  def ownIndices                          = range.indices filter owns
  def childIndices                        = range.indices filterNot owns
  def allRanges: Seq[(IntRange, Boolean)] = IntRanges(ownIndices).map(_ -> true) ++ IntRanges(childIndices).map(_ -> false) sortBy (_._1.start)
}

// abstract class AssignCoverage[T](root: T) {
//   def rangeOf(node: T): IntRange
//   def childrenOf(node: T): Seq[T]

//   def nodeList(node: T): Seq[T] = node :: (childrenOf(node) flatMap nodeList)
//   def nodeMap[U](node: T)(f: T => U): Seq[U] = nodeList(node) map f

//   def hasChildWhich(node: T)(p: T => Boolean) = childrenOf(node) exists p
//   def covers(node: T, index: Int) = rangeOf(node) contains index
//   def owns(node: T, index: Int)     = contains(node, index) && !hasChildWhich(node)(contains(c, _))

//   def ownsDirectly(node: T, index: Int) = (rangeOf(node) contains index) && !(childrenOf(node) exists (c => rangeOf(c) contains index))

//   val nodes       = nodeList(root)
//   val overlapping = nodes map rangeOf
//   val boundaries  = overlapping.flatMap(r => r.start :: r.end :: Nil).distinct.sorted
//   val disjoint    =
// }

class TreePosAnalyzer[U <: Global](val u: U) {
  import u._
  import syntaxAnalyzer._

  private def codeFor(t: Tree, maxlen: Int): String = (
    if (t.pos.isOpaqueRange)
      cleanup(t.pos.source.content.slice(t.pos.start, t.pos.end).mkString, maxlen)
    else
      cleanup("" + t, maxlen)
  )
  private def cleanup(s: String, maxlen: Int): String = {
    val lines = s.lines.toVector
    val str0 = (
      if (lines.size == 1) lines.head
      else {
        val braceIndices = (lines.indices filter (idx => (lines(idx).trim endsWith "{") || (lines(idx).trim endsWith "}"))).toSet
        lines.zipWithIndex map { case (l, i) => if (braceIndices(i)) l + " " else l + "; " } mkString ""
      }
    )
    val str = str0.replaceAll("""[;]+""", ";")
    if (str.length <= maxlen) str
    else {
      val half = (maxlen - 5) / 2
      (str take half) + " ... " + (str takeRight half)
    }
  }

  class AssignCoverage(unit: CompilationUnit) {
    val sourceCode = unit.source.content.mkString
    def codeSlice(range: IntRange): String = sourceCode.slice(range.start, range.end).mkString

    def getCode(node: WNode): String = {
      val chunks = node.allRanges map { case (range, own) => CString(cleanup(codeSlice(range), 50))(if (own) Console.GREEN else "") }
      val totalLen = chunks.map(_.length).sum

      if (totalLen <= 50) chunks mkString ""
      else if (chunks.size == 1) cleanup(chunks.head.toString, 50)
      else chunks.head + " ... " + chunks.last
    }

    // def colored = IntRanges(n.ownIndices)

    val root     = WNode(unit.body)
    val nodes    = nodeList(root)
    val indexMap = immutable.IntMap(nodes.flatMap(n => n.ownIndices.map(idx => idx -> n)): _*)
    // val code  = cleanup(code.slice(c.start, c.end).mkString, 50))
    val lines    = nodes map (n => "[%5s]  %-70s // #%-5s %-20s %s".format(n.pos.start, getCode(n), n.tree.id, n.tree.shortClass, IntRanges(n.ownIndices).mkString("~")))

    //   lines.mkString(unit.source.file.name + "\n", "\n", "\n")

    def apply(index: Int): WNode = indexMap(index)

    private def nodeList(node: WNode): Seq[WNode] = node +: (node.children flatMap nodeList)

    override def toString = lines.mkString(unit.source.file.name + "\n", "\n", "\n")
  }

  case class WNode(tree: Tree) extends Node[WNode] {
    def pos      = tree.pos
    def range    = IntRange(pos.start, pos.end)
    def children = tree.children filter (_.pos.isOpaqueRange) map WNode
  }

  def chunkUnit(unit: CompilationUnit): String = {
    new AssignCoverage(unit).toString
  }


  // object treeCoverage extends TreeCoverage {
  //   type Tree = u.Tree

  //   def EmptyTree                         = u.EmptyTree
  //   def childrenOf(tree: Tree): Seq[Tree] = tree.children
  //   def positionOf(tree: Tree): Position  = tree.pos
  //   def identOf(tree: Tree): Int          = tree.id
  //   def isIndentingTree(tree: Tree)       = tree match {
  //     case _: MemberDef | _: Block => true
  //     case _                       => false
  //   }
  // }

  // def ownOffsets(tree: Tree): Vector[Offset] = {
  //   val exclude = (tree.children map positionOf filter (_.isOpaqueRange) flatMap (_.indices)).toSet
  //   (positionOf(tree).indices filterNot exclude).toVector
  // }
  // def sortedTrees(tree: Tree): Seq[Tree] = {
  //   tree filter (_.pos.isOpaqueRange) sortBy (_.pos)
  // }
  // def getCode(t: Tree): String = {
  //   val childIndices = t.children map positionOf filter (_.isOpaqueRange) flatMap (_.indices)
  //   val ownIndices   = t.pos.indices filterNot childIndices.toSet

  //   def chunks(indices: Vector[Int]): Vector[String] = {

  //   }

  // }

  // def chunkUnit(unit: CompilationUnit): String = {
  //   val code       = unit.source.content.mkString
  //   val trees      = sortedTrees(unit.body)
  //   val boundaries = trees.flatMap(t => t.pos.start :: t.pos.end :: Nil).distinct.sorted
  //   val coverage   = trees map (t => t.

  //   val boundaryMap = boundaries map (idx =>

  //   val snippets = trees map (t => cleanup(code.slice(t.pos.start, t.pos.end).mkString, 50))

  //   val chunks   = treeCoverage.analyze(code, unit.body) filterNot (_.tree == EmptyTree)
  //   val snippets = chunks map (c => cleanup(code.slice(c.start, c.end).mkString, 50))
  //   val lines    = (snippets, chunks).zipped map ((s, c) => "[%5s]  %-70s // #%-5s %s".format(c.start, s, c.tree.id, c.tree.shortClass))

  //   lines.mkString(unit.source.file.name + "\n", "\n", "\n")
  // }

     // chunks map (c => "%-50s // %s".format(codeOf(c), shortClassOfInstance(coverage(c.offset)))) mkString "\n"

  // case class ChunkedSource(sourceCode: String, chunks: Vector[Chunk]) {
  //   def codeOf(chunk: Chunk): String = sourceCode.slice(chunk.start, chunk.end).mkString

  //   override def toString = chunks map (c => "%-50s // %s".format(codeOf(c), shortClassOfInstance(coverage(c.offset)))) mkString "\n"
  // }


  class TreeData(val unit: CompilationUnit) {
    val treeParents  = mutable.Map[Tree, Tree]() withDefaultValue EmptyTree
    val treeChildren = mutable.Map[Tree, List[Tree]]() withDefaultValue Nil
    val badTrees     = mutable.Map[Tree, List[String]]() withDefaultValue Nil

    implicit object TreeOrdering extends Ordering[Tree] {
      def compare(t1: Tree, t2: Tree): Int = (
        if (t1 == t2) 0
        else if (parentChain(t2) contains t1) -1
        else if (parentChain(t1) contains t2) 1
        else t1.pos compare t2.pos
      )
    }

    def areSiblings(t1: Tree, t2: Tree) = opaqueParent(t1) == opaqueParent(t2)

    def classAndRange(t: Tree) = s"${t.shortClass}${t.pos.showNoPoint}"

    def checkChildren(t: Tree) {
      val kids = treeChildren(t)
      for (c1 <- kids ; c2 <- kids ; if TreeOrdering.lt(c1, c2) && areSiblings(c1, c2) && (c1.pos overlaps c2.pos)) yield {
        val xs      = c1.pos.indices intersect c2.pos.indices
        val len     = xs.max - xs.min + 1
        val message = "%s+%s".format(xs.min, len)
        val parentMessage = kids map (t => if (t == c1 || t == c2) inRed(classAndRange(t)) else classAndRange(t)) mkString (" ", " ~ ", " ")

        badTrees(t) ::= parentMessage
        badTrees(c1) ::= message
        badTrees(c2) ::= message
      }
    }
    def checkParents(t: Tree) {
      properParentChain(t) filterNot (_.pos includes t.pos) foreach { p =>
        badTrees(t) ::= s"parent ${p.shortClass}${p.pos.showNoPoint} !includes ${t.shortClass}${t.pos.showNoPoint}"
      }
    }

    def properParentChain(t: Tree) = parentChain(t) drop 1
    def parentChain(t: Tree): List[Tree] = treeParents get t match {
      case Some(parent) => t :: parentChain(parent)
      case _            => t :: Nil
    }
    def opaqueChildren(t: Tree): List[Tree] = (
      t.children.toList flatMap (t =>
        if (t.pos.isOpaqueRange) t :: Nil
        else if (t.pos.isTransparent) t.children.toList flatMap opaqueChildren
        else Nil
      )
    )//.sortWith((t1, t2) => t2.pos < t1.pos)

    def opaqueParent(t: Tree): Tree = properParentChain(t) find (_.pos.isOpaqueRange) getOrElse EmptyTree

    locally {
      unit.body foreach { t =>
        val kids = opaqueChildren(t)
        if (kids.nonEmpty)
          treeChildren(t) = kids
      }
      treeChildren.keys foreach (k =>
        treeChildren(k) foreach (v =>
          parentChain(k) find (_.pos.isOpaqueRange) foreach (p =>
            treeParents(v) = p
          )
        )
      )
    }
  }

  class TreeAnalysis(unit: CompilationUnit) {
    val source       = unit.source
    val length       = content.length
    val tokens       = readTokens()
    val covered      = readCovered()
    val chunks       = readChunks()
    val chunkIndices = chunks.indices.toVector
    val treeData     = new TreeData(unit)

    import treeData._

    def content = source.content
    def body    = unit.body

    case class Chunk[T](start: Int, length: Int, value: T) {
      def end     = start + length
      def indices = start until end
      def code    = content.slice(start, end).mkString
    }
    case class OpenCondition(index: Int, depth: Int)

    private def readChunks(): Vector[Chunk[Vector[Tree]]] = {
      def loop[T](chunks: Vector[Chunk[T]], remaining: Seq[T]): Vector[Chunk[T]] = {
        if (remaining.isEmpty)
          return chunks

        val headValue = remaining.head
        val startIndex = if (chunks.isEmpty) 0 else chunks.last.end
        val (hd, tl) = remaining span (_ == headValue)
        val chunk = Chunk(startIndex, hd.length, headValue)
        loop(chunks :+ chunk, tl)
      }
      loop(Vector(), covered.toVector)
    }

    private def readCovered(): Vector[Vector[Tree]] = {
      val indexMap = (
        body filter (_.pos.isOpaqueRange)
          flatMap (t => t.pos.indices.map(_ -> t))
          groupBy (_._1)
          mapValues (_.map(_._2).toVector)
      )
      content.indices.toVector map (idx => indexMap.getOrElse(idx, Vector()))
    }

    private def readTokens(): Vector[TokenInfo] = {
      val buf = mutable.ListBuffer[TokenInfo]()

      class SFS extends SourceFileScanner(source) {
        init()
        def token2source(token: Int): SourceToken = (token: @switch) match {
          case IDENTIFIER | BACKQUOTED_IDENT            => SourceTokens.Ident(name.decoded)
          case CHARLIT                                  => SourceTokens.Literal(charVal)
          case INTLIT | LONGLIT                         => SourceTokens.Literal(intVal)
          case FLOATLIT | DOUBLELIT                     => SourceTokens.Literal(floatVal)
          case STRINGLIT | STRINGPART | INTERPOLATIONID => SourceTokens.Literal(strVal)
          case SYMBOLLIT                                => SourceTokens.Literal(scala.Symbol(strVal))
          case _                                        => SourceTokens.Keyword(token)
        }
      }
      val s = new SFS

      @tailrec def loop() {
        val off   = new SourceTokens.Offset(s.charOffset)
        val token = TokenInfo(off, s.token2source(s.token))
        buf += token
        s.nextToken()
        if (s.charOffset < length)
          loop()
      }

      loop()
      val tokens = buf.toVector
      val rate = "%.2f" format length.toDouble / tokens.size
      val path = {
        val segments = source.path.toString split '/'
        segments indexWhere (_ == "scala") match {
          case -1  => segments takeRight 3 mkString "/"
          case idx => segments drop idx mkString "/"
        }
      }
      println(f"$length%5sc ${tokens.size}%5st $rate%6s c/t  $path")
      tokens
    }

    class DisplayTraverser() {
      var lastOpened: Tree = EmptyTree
      val displayed = mutable.Set[Tree]()
      val opened    = mutable.Map[Tree, Int]()
      val fmt       = "%6s%1s%-6s  %-40s  %s"
      def openTrees = opened.keys.toList.sorted.reverse
      def openDepth = openTrees.size

      def closeDeadTree(tree: Tree, chunkIndex: Int) {
        val distance = chunkIndex - opened(tree)
        val showClose = distance > 1
        opened -= tree
        val indent = " | " * openDepth + " \\-|"
        if (showClose)
          println(fmt.format("", "", "", "", indent))
      }

      def nextChunkIndex(chunkIndex: Int) {
        def chunk        = chunks(chunkIndex)
        def currentTrees = chunk.value.sorted
        def newTrees     = currentTrees filterNot openTrees.contains
        def deadTrees    = openTrees filterNot currentTrees.contains

        def openNewTree(t: Tree) {
          checkChildren(t)
          checkParents(t)
          // println(parentChain(t).map(classAndRange).mkString(" -> "))

          val indent    = " | " * openDepth
          val message   = badTrees(t) take 1 map (m => if (m contains Console.RESET) s"[$m]"else inRed(s"[$m]")) mkString ""
          val posString = "" + t.pos.length
          val str       = s"%s%s[%s]%s".format(indent, t.shortClass, posString, message)
          val code      = if (lastOpened.pos sameRange t.pos) "" else codeFor(t, 40)
          val toPrint = fmt.format(t.pos.start, ":", t.pos.end, code, str)
          println(toPrint)
          lastOpened = t
          displayed += t
          opened(t) = chunkIndex
        }

        deadTrees foreach (closeDeadTree(_, chunkIndex))
        newTrees filterNot opened.contains foreach openNewTree
      }

      def display() {
        chunkIndices foreach nextChunkIndex
        opened.keys.toList.sorted.reverse foreach (t => closeDeadTree(t, chunkIndices.last))
      }
    }

    def analyze(): Unit = (new DisplayTraverser).display()
  }

  def validate(unit: CompilationUnit) {
    val analysis = new TreeAnalysis(unit)
    analysis.analyze()
  }
}

object TreePosAnalyzer {
  def isAnalyze = sys.props contains "analyze"
  def apply(u: Global)(unit: u.CompilationUnit) {
    if (isAnalyze) {
      val analyzer = new TreePosAnalyzer[u.type](u)
      println(analyzer chunkUnit unit)
      // analyzer validate unit
    }
  }
}
