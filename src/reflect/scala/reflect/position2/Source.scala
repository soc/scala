package scala
package reflect
package position

import scala.reflect.internal.util._
import scala.reflect.internal.Chars._

trait TextView extends Any {
  def asCharArray: Array[Char]
  def asLineArray: Array[String]
  def asString: String
}

trait Source extends Any {
  def name: String
  def path: String

  def lengthInChars: LengthInChars
  def lengthInLines: LengthInLines

  def viewChar(index: CharOffset): Char
  def viewLine(index: LineNumber): String
  def viewChars(range: CharRange): TextView
  def viewLineAt(offset: CharOffset): String

  def offsetToCoordinates(offset: CharOffset): RowAndColumn
  def offsetToLine(offset: CharOffset): LineNumber
  def lineToOffset(line: LineNumber): CharOffset
}

object Source {
  // implicit def raiseSourceFile(file: SourceFile): SourceFileSource         = new SourceFileSource(file)
  // implicit def lowerSourceFileSource(source: SourceFileSource): SourceFile = source.source
}

class SourceFileSource(val source: SourceFile) extends Source {
  def this(file: java.io.File) = this(new BatchSourceFile(new scala.reflect.io.PlainFile(file)))
  def this(path: String) = this(new java.io.File(path))

  private def chars = source.content
  private implicit def liftIndex(x: Int): CharOffset = Index(x)
  private val newlineIndices = chars.indices filter isLineBreak
  // private val offsetsForLines = 0 +: newlineIndices.map(_ + 1)
  private val lineIndices = (0 +: newlineIndices.map(_ + 1) :+ chars.length).distinct

  val lines: Vector[String] = if (lineIndices.size < 2) Vector() else calcLines()
  def charArray = chars.toArray

  // don't identify the CR in CR LF as a line break, since LF will do.
  def isLineBreak(idx: Int) = chars(idx) match {
    case CR => (idx + 1 == lengthInChars.value) || (chars(idx + 1) != LF)
    case ch => isLineBreakChar(ch)
  }

  private def calcLines(): Vector[String] = {
    val lines = {
      for (start :: end :: Nil <- lineIndices sliding 2 map (_.toList)) yield
        chars.slice(start, end).mkString.stripLineEnd
    }
    lines.toVector
  }



  //   var i = 0
  //   var head = 0
  //   var until = 0
  //   var buf: List[String] = Nil

  //   while (i < newlineIndices.length) {
  //     until = newlineIndices(i)
  //     buf ::= chars.slice(head, until).mkString("").stripLineEnd
  //     head = until + 1
  //     i += 1
  //   }
  //   if (head < chars.length)
  //     buf ::= chars.slice(head, buf.length).mkString("")

  //   buf.reverse.toVector
  // }

    // var buf: List[String] = slice(0, newlineIndices(0))
    // while (i < newlineIndices.length) {
    //   buf ::= slice(
  // private val lines: Vector[String] = (offsetsForLines, offsetsForLines.tail map (_ - 1)).zipped map ((start, end) => chars.slice(start, end).mkString) toVector;

  // private def applyLineNumber[T](xs: Seq[T], line: LineNumber): T = xs(line.value - 1)

  // private def newlineIndices: Vector[Int] = chars.indices.toVector filter (idx => chars(idx) == '\n')
  // private val offsetsForLines: Vector[Int] = 0 +: newlineIndices.map(_ + 1)
  // private val endOffsetsForLines = newlineIndices
  // private

  // private val offsetsForLines: Vector[CharOffset] = intOffsetsForLines map (x => x)

  private def view(range: CharRange): TextView                   = view(range.start, range.end)
  private def view(start: CharOffset, end: CharOffset): TextView = new SFSTextView(start, end)

  private class SFSTextView(start: CharOffset, end: CharOffset) extends TextView {
    def asCharArray       = chars.slice(start.value, end.value)
    def asLineArray       = asString.lines.toArray
    def asString          = asCharArray mkString ""
    override def toString = asString
  }

  if (source ne NoSourceFile) {
    if (sys.props contains "dump") {
      // (new Throwable).printStackTrace
      println(path)
      println(s"""
        |$path
        |   chars: $lengthInChars
        |   lines: $lengthInLines
        |  source:
        |""".stripMargin + "\n" + annotatedSource + "\n")
    }
  }

  def annotatedLines = (lineIndices, lineIndices.tail map (_ - 1)).zipped map ((start, end) => f"[$start%5s-$end%-5s] " + chars.slice(start, end).mkString(""))
  def annotatedSource = annotatedLines mkString "\n"

  def name: String          = source.file.name
  def path: String          = source.file.path

  def lengthInChars: LengthInChars = Length[Char](chars.length)
  def lengthInLines: LengthInLines = Length[LineNumber](lineIndices.length)

  def viewChar(index: CharOffset): Char      = chars(index.index)
  def viewChars(range: CharRange): TextView  = view(range)
  def viewLineAt(offset: CharOffset): String = viewLine(offsetToLine(offset))

  def viewLine(line: LineNumber): String     = lines(line.index)

  def viewLines(start: Int, end: Int): TextView = view(lineToOffset(LineNumber(start)), lineToOffset(LineNumber(end)))
  def viewChars(start: Int, end: Int): TextView = view(Index(start), Index(end))

  private def coordinatesFor(offset: CharOffset): RowAndColumn = {
    lineIndices indexWhere (offset.value < _) match {
      case -1    => RowAndColumn.Empty
      case index =>
        val lineNumber = LineNumber(index)
        val start      = lineIndices(lineNumber.index)
        val col        = offset.value - start + 1

        RowAndColumn(lineNumber.value, col)
    }
  }

        // val col        = processTabs(lineChars).length + 1
        // val lineChars  = chars.slice(start, offset.value).mkString

  //   // println(s"""coordinatesFor($offset) / $lineIndices""")
  //   val lineNumber = LineNumber(lineIndices indexWhere (offset.value < _))

  //   // val max       = offset.value
  //   // // val lineIndex = offsetToLineOld(offset.value)
  //   // val lineIndex = lineIndices indexWhere (_ > max) match {
  //   //   case -1  => lengthInLines.value - 1
  //   //   case idx => idx - 1
  //   // }
  //   val start = lineIndices(lineNumber.index)
  //   val lineChars = chars.slice(start, offset.value).mkString
  //   val col = processTabs(lineChars).length + 1
  //   // val col   = offset.value - start + 1
  //   val res   = RowAndColumn(lineNumber.value, col)
  //   // println(s"offset=$offset, lineNumber=$lineNumber, start=$start, col=$col, res=$res")
  //   res
  // }
    // val max = offset.index
    // def loop(line: LineNumber): RowAndColumn = {
    //   val lineStart = lineToOffset(line)
    //   val nextStart = lineToOffset(line.next)
    //   println(s"loop($line): lineStart=$lineStart, nextStart=$nextStart")

    //   if (nextStart.index > max)
    //     RowAndColumn(line.index, max - lineStart.index + 1)
    //   else
    //     loop(line.next)
    // }
    // loop(LineNumber(1))

  private def offsetToLineOld(offset: Int): Int = {
    val lines = lineIndices
    def findLine(lo: Int, hi: Int, mid: Int): Int = (
      if (offset < lineIndices(mid)) findLine(lo, mid - 1, (lo + mid - 1) / 2)
      else if (offset >= lineIndices(mid + 1)) findLine(mid + 1, hi, (mid + 1 + hi) / 2)
      else mid
    )
    findLine(0, lineIndices.length, 0)
  }

  def offsetToCoordinates(offset: CharOffset): RowAndColumn = coordinatesFor(offset)
  def offsetToLine(offset: CharOffset): LineNumber          = LineNumber(coordinatesFor(offset).row)
  def lineToOffset(line: LineNumber): CharOffset            = Index(lineIndices(line.index - 1))

  override def toString = source.toString
}
