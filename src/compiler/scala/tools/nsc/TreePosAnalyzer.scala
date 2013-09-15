package scala
package tools
package nsc

import scala.reflect.internal.util._
import scala.reflect.internal.Chars._
import scala.tools.nsc.ast.parser.Tokens._
import scala.collection.{ mutable, immutable }
import scala.annotation.{ switch, tailrec }

object SourceTokens {
  def inRed(s: String)          = Console.RED ++ Console.BOLD + s + Console.RESET

  sealed abstract class SourceToken {
    def stringValue: String
    def color: String
    override def toString = color + Console.BOLD + stringValue + Console.RESET
  }

  def keywordString(token: Int): String = token match {
    case ABSTRACT   => "abstract"
    case ARROW      => "=>"
    case AT         => "@"
    case CASE       => "case"
    case CASECLASS  => "case class"
    case CASEOBJECT => "case object"
    case CATCH      => "catch"
    case CLASS      => "class"
    case COLON      => ":"
    case COMMA      => ","
    case COMMENT    => "//"
    case DEF        => "def"
    case DO         => "do"
    case DOT        => "."
    case ELSE       => "else"
    case EOF        => "<eof>"
    case ERROR      => "<error>"
    case EQUALS     => "="
    case EXTENDS    => "extends"
    case FALSE      => "false"
    case FINAL      => "final"
    case FINALLY    => "finally"
    case FOR        => "for"
    case FORSOME    => "forSome"
    case HASH       => "#"
    case IF         => "if"
    case IMPLICIT   => "implicit"
    case IMPORT     => "import"
    case LARROW     => "<-"
    case LAZY       => "lazy"
    case LBRACE     => "{"
    case LBRACKET   => "["
    case LPAREN     => "("
    case MACRO      => "macro"
    case MATCH      => "match"
    case NEW        => "new"
    case NEWLINE    => "<nl>"
    case NEWLINES   => "<nls>"
    case NULL       => "null"
    case OBJECT     => "object"
    case OVERRIDE   => "override"
    case PACKAGE    => "package"
    case PRIVATE    => "private"
    case PROTECTED  => "protected"
    case RBRACE     => "}"
    case RBRACKET   => "]"
    case RETURN     => "return"
    case RPAREN     => ")"
    case SEALED     => "sealed"
    case SEMI       => ";"
    case SUBTYPE    => "<:"
    case SUPER      => "super"
    case SUPERTYPE  => ">:"
    case THEN       => "then"
    case THIS       => "this"
    case THROW      => "throw"
    case TRAIT      => "trait"
    case TRUE       => "true"
    case TRY        => "try"
    case TYPE       => "type"
    case USCORE     => "_"
    case VAL        => "val"
    case VAR        => "var"
    case VIEWBOUND  => "<%"
    case WHILE      => "while"
    case WHITESPACE => "<ws>"
    case WITH       => "with"
    case YIELD      => "yield"
    case _          => s"<token=$token>"
  }

  def identifier(name: String): Ident              = Ident(name)
  def keyword(token: Int, string: String): Keyword = Keyword(token, string)

  case class Keyword(token: Int, stringValue: String) extends SourceToken {
    def color = Console.WHITE
  }
  case class Ident(stringValue: String) extends SourceToken {
    def color = Console.CYAN
  }
  case class Literal(value: Any) extends SourceToken {
    def color = Console.GREEN
    def stringValue = "" + value
  }

  final case class Offset(val offset: Int) extends AnyVal

  case class TokenInfo(start: Offset, token: SourceToken)
}
import SourceTokens._

class TreePosAnalyzer[U <: Global](val u: U) {
  import u._
  import syntaxAnalyzer._

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
          case IDENTIFIER | BACKQUOTED_IDENT            => identifier(name.decoded)
          case CHARLIT                                  => SourceTokens.Literal(charVal)
          case INTLIT | LONGLIT                         => SourceTokens.Literal(intVal)
          case FLOATLIT | DOUBLELIT                     => SourceTokens.Literal(floatVal)
          case STRINGLIT | STRINGPART | INTERPOLATIONID => SourceTokens.Literal(strVal)
          case SYMBOLLIT                                => SourceTokens.Literal(scala.Symbol(strVal))
          case _                                        => keyword(token, keywordString(token))
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

  //     // def loop[T](chunks: Vector[Chunk[T]], remaining: Seq[T]): Vector[Chunk[T]] = {
  //     //   if (remaining.isEmpty)
  //     //     return chunks

  //     //   val headValue = remaining.head
  //     //   val startIndex = if (chunks.isEmpty) 0 else chunks.last.end
  //     //   val (hd, tl) = remaining span (_ == headValue)
  //     //   val chunk = Chunk(startIndex, hd.length, headValue)
  //     //   loop(chunks :+ chunk, tl)
  //     // }
  //     // val chunks          = loop[Vector[Tree]](Vector(), covered.toVector)
  //     // val open            = mutable.Map[Tree, OpenCondition]()

  //     val opened = mutable.Map[Tree, Int]()
  //     val displayedChunks = mutable.Set[Int]()
  //     val fmt             = "%-3s%6s%1s%-6s  %-40s  // %s"

  //     // def chunk          = chunks(chunkIndex)
  //     // def openDepth      = open.size
  //     // def enclosingTrees = open.keys.toList.sorted
  //     // def enclosingTrees = (currentTrees flatMap properParentChain).distinct
  //     // def currentTrees   = chunk.value.sorted
  //     // def ongoingTrees   = currentTrees filter isEnclosing
  //     // def newTrees       = currentTrees filterNot isEnclosing
  //     // def deadTrees      = enclosingTrees filterNot isLive
  //     def marker         = ""

  //     // def isEnclosing(t: Tree) = enclosingTrees contains t
  //     // def isLive(t: Tree)      = currentTrees contains t

  //     val opened = mutable.Map[Tree, Int]()
  //   }
  // }

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

  // def rangeString(pos: Position): String = {
  //   val ldelim = if (pos.isTransparent) "<" else "["
  //   val rdelim = if (pos.isTransparent) ">" else "]"

  //   ldelim + ("%5s%-6s".format(pos.start, if (pos.start == pos.end) "" else "-" + pos.end)) + rdelim
  // }

  // def makeLine(t: Tree, depth: Int): String = {
  //   def what = if (t.pos.isOpaqueRange) "src" else "ast"
  //   def tstr = codeFor(t, 65)
  //   val lhs  = "%-15s %s %s".format(rangeString(t.pos), "  " * depth, t.shortClass)
  //   val rhs  = s"[$what]  $tstr"

  //   "%-40s  // %s".format(lhs, rhs)
  // }

  def validate(unit: CompilationUnit) {
    val analysis = new TreeAnalysis(unit)
    analysis.analyze()

    // setSources(unit)
    // if (TreePosAnalyzer.isAnalyze)
    //   validate(unit.body)
  }

  // def validate(t: Tree) {
  //   def show(t: Tree, depth: Int) {
  //     println(makeLine(t, depth))
  //     if (t.pos.isTransparent)
  //       t.children.toList foreach (c => show(c, depth + 1))
  //   }
  //   def opaque = opaqueChildren(t)

  //   if (opaque.nonEmpty) {
  //     println(makeLine(t, 0))
  //     t.children.toList filter (_.pos.isDefined) foreach (k => show(k, 1))
  //     println("")
  //     opaque foreach validate
  //   }
  // }
}

object TreePosAnalyzer {
  def isAnalyze = sys.props contains "analyze"
  def apply(u: Global)(unit: u.CompilationUnit) {
    if (isAnalyze)
      new TreePosAnalyzer[u.type](u) validate unit
  }
}



  // chunks foreach { chunk =>
  //   chunkIndex += 1


    // { d =>
    //   println(fmt.format(marker, "", "", "", "", (" | " * openDepth) + " \\-|"))
    // }

    // // val deadTrees             = enclosingTrees filterNot isLive
    // // val (openTrees, newTrees) = currentTrees partition isOpen

    // deadTrees foreach { d =>
    //   open -= d
    //   println(fmt.format(marker, "", "", "", "", (" | " * openDepth) + " \\-|"))
    // }
    // newTrees foreach (t => openNewTree(c, chunkIndex, t))

    // val overlaps = newTrees exists (t1 =>
    //   openTrees exists (t2 =>
    //     (t1.pos overlapsBadly t2.pos) && {
    //       println(t1.pos.show + " overlaps " + t2.pos.show)
    //       true
    //     }
    //   )
    // )
    // val overlapping = {
    //   newTrees flatMap { t1 =>
    //     val kids = opaqueChildren(t1).toList
    //     val siblings = (
    //       for (c1 <- kids ; c2 <- kids ; if c1.pos < c2.pos && (c1.pos overlaps c2.pos)) yield {
    //         val xs      = c1.pos.indices intersect c2.pos.indices
    //         val len     = xs.max - xs.min + 1
    //         val message = "%s+%s".format(xs.min, len)
    //         val parentMessage = kids map (t => if (t == c1 || t == c2) inRed(t.pos.showNoPoint) else t.pos.showNoPoint) mkString (" ", " ~ ", " ")

    //         badChildren(t1) = parentMessage
    //         badChildren(c1) = message
    //         badChildren(c2) = message
    //         // println("badIndices = " + badIndices)
    //         // badChildren ++= List(c1, c2)
    //         c1 -> c2
    //       }
    //     )

    //     val parentChild = (
    //       for (p1 <- open.keys.toList ; if !(p1.pos includes t1.pos)) yield {
    //         badChildren(t1) = s"child #${t1.id} not contained in enclosing ${p1.shortClass}"
    //         t1 -> p1
    //       }
    //     )

    //     siblings ++ parentChild
    //   }
    // }
    // val marker = if (overlaps) "o!" else if (overlapping.nonEmpty) "c!" else ""

    // val includesOk = {
    //   (currentTrees.length < 2) || (currentTrees sliding 2 forall { xs =>
    //     val t1 :: t2 :: Nil = xs.toList
    //     t1.pos includes t2.pos
    //   })
    // }
    // (deadTrees map open map (_.depth)).sorted.reverse foreach { d =>
    //   println(fmt.format("", "", "", "", (" | " * d) + "--"))
    // }

    // newTrees.zipWithIndex foreach { (t, idx) =>
    //   val indent = "  " * (depth - newTrees.size + idx)

    // }

    // val indent = "  " * (depth - newTrees.length)
    // val treeString = newTrees map (t => s"%s[%s]".format(t.shortClass, t.pos.length)) mkString (indent, " / ", "")
    // val str = poses map ("%-15s" format _.show) mkString " / "
    // val skip = newTrees.isEmpty //&& (c.indices forall (i => content(i).isWhitespace))


    // def show(newTree: Tree, isFirst: Boolean) {
    //   val indent = " | " * openDepth
    //   val message = badChildren get newTree match {
    //     case Some(message) if message contains Console.RESET => "[" + message + "]"
    //     case Some(message)                                   => inRed("[" + message + "]")
    //     case _                                               => ""
    //   }
    //   val str    = s"%s%s[%s]%s".format(indent, newTree.shortClass, newTree.pos.length, message)
    //   if (isFirst)
    //     println(fmt.format(marker, c.start, "+", c.length, cleanup(c.code, 40), str))
    //   else
    //     println(fmt.format(marker, "  ...", "", "", "", str))

    //   if (newTree.pos.end > c.end)
    //     open(newTree) = OpenCondition(chunkIndex, openDepth)
    // }

    // newTrees.toList match {
    //   case Nil     =>
    //   case x :: xs =>
    //     show(x, isFirst = true)
    //     xs foreach (x => show(x, isFirst = false))
    // }
//   }

//   ""
// }

// private def setSources(unit: CompilationUnit) {
//   source = unit.source
//   content = unit.source.content
//   val buf = mutable.ListBuffer[TokenInfo]()
//   val len = content.length

//   class SFS extends SourceFileScanner(source) {
//     init()
//     def token2source(token: Int): SourceToken = (token: @switch) match {
//       case IDENTIFIER | BACKQUOTED_IDENT            => identifier(name.decoded)
//       case CHARLIT                                  => SourceTokens.Literal(charVal)
//       case INTLIT | LONGLIT                         => SourceTokens.Literal(intVal)
//       case FLOATLIT | DOUBLELIT                     => SourceTokens.Literal(floatVal)
//       case STRINGLIT | STRINGPART | INTERPOLATIONID => SourceTokens.Literal(strVal)
//       case SYMBOLLIT                                => SourceTokens.Literal(scala.Symbol(strVal))
//       case _                                        => keyword(token, keywordString(token))
//     }
//   }
//   val s = new SFS

//   @tailrec def loop() {
//     val off   = new SourceTokens.Offset(s.charOffset)
//     val token = TokenInfo(off, s.token2source(s.token))
//     buf += token
//     s.nextToken()
//     if (s.charOffset < len)
//       loop()
//   }

//   loop()
//   tokens = buf.toVector
//   val rate = "%.2f" format len.toDouble / tokens.size
//   val path = {
//     val segments = source.path.toString split '/'
//     segments indexWhere (_ == "scala") match {
//       case -1  => segments takeRight 3 mkString "/"
//       case idx => segments drop idx mkString "/"
//     }
//   }
//   println(f"$len%5sc ${tokens.size}%5st $rate%6s c/t  $path")

//   analyzePositions(units)

//   // println(s"$source is $len chars, ${tokens.size} tokens, $rate chars/token")
//   // for ((TokenInfo(Offset(off), token), idx) <- tokens.zipWithIndex) {
//   //   println(f"$idx%3d  +$off%-5d  $token")
//   // }
// }

