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

  object treeCoverage extends TreeCoverage {
    type Tree = u.Tree

    def EmptyTree                         = u.EmptyTree
    def childrenOf(tree: Tree): Seq[Tree] = tree.children
    def positionOf(tree: Tree): Position  = tree.pos
    def identOf(tree: Tree): Int          = tree.id
    def isIndentingTree(tree: Tree)       = tree match {
      case _: MemberDef | _: Block => true
      case _                       => false
    }
  }

  def chunkUnit(unit: CompilationUnit): String = {
    val code     = unit.source.content.mkString
    val chunks   = treeCoverage.analyze(code, unit.body) filterNot (_.tree == EmptyTree)
    val snippets = chunks map (c => cleanup(code.slice(c.start, c.end).mkString, 50))
    val lines    = (snippets, chunks).zipped map ((s, c) => "[%5s]  %-70s // #%-5s %s".format(c.start, ("  " * c.depth.value) + s, c.tree.id, c.tree.shortClass))

    lines.mkString(unit.source.file.name + "\n", "\n", "\n")
  }

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
