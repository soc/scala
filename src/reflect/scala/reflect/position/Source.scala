package scala
package reflect
package position

import scala.reflect.internal.util._

trait TextView extends Any {
  def asCharArray: Array[Char]
  def asLines: Vector[String]
  def asString: String
}

trait Source extends Any {
  def name: String
  def path: String
  def chars: IndexedSeq[Char]
  def lines: IndexedSeq[String]
  def lineIndices: IndexedSeq[Index]
  def coordinatesOf(offset: Index): RowAndColumn
}

object NoSource extends Source {
  val name                         = "<none>"
  val path                         = ""
  val chars                        = Vector()
  val lines                        = Vector()
  val lineIndices                  = Vector()
  def coordinatesOf(offset: Index) = RowAndColumn.Empty
}

class FileSource(val file: java.io.File, val chars: ImmutableChars) extends Source {
  val lines       = chars.lines
  val lineIndices = chars.lineIndices

  if (file.length > 0 && (sys.props contains "dump")) this.dump()

  def name        = file.getName
  def path        = file.getPath
  def coordinatesOf(offset: Index): RowAndColumn = lineIndices indexWhere offset.< match {
    case -1    => RowAndColumn.Empty
    case index => RowAndColumn(index, (lineIndices(index - 1) to offset).length)
  }
  override def toString = file.toString
}

object Source {
  private def charsOf(arr: Array[Char]) = if (arr eq null) ImmutableChars(Array[Char]()) else ImmutableChars(arr)

  def apply(file: java.io.File): Source = if (file eq null) NoSource else new FileSource(file, ImmutableChars(file))
  def apply(file: SourceFile): Source   = if (file.file.file eq null) NoSource else new FileSource(file.file.file, charsOf(file.content))
  def apply(path: String): Source       = apply(new java.io.File(path))

  implicit final class SourceDebugOps(val source: Source) extends AnyVal {
    import source._
    private def ljust(max: Int): String = "%%-%ds" format max.toString.length
    private def rjust(max: Int): String = "%%%ds" format max.toString.length

    private def lineMarkers = {
      val lineFmt  = rjust(lines.length)
      val startFmt = rjust(chars.length)
      val endFmt   = ljust(chars.length)
      val fmt      = List("[", lineFmt, "  ", startFmt, "-", endFmt, "]  ").mkString

      source.lineNumbers map { l =>
        val start = source lineToOffset l
        val end   = start add (source lineAt l).length
        fmt.format(l.value, start, end)
      }
    }

    def annotatedLines  = (lineMarkers, lines).zipped map (_ + _)
    def annotatedSource = annotatedLines mkString "\n"
    def dump(): Unit = println(s"""
      |$path
      |   chars: ${chars.length}
      |   lines: ${lines.length}
      |  source:
      |$annotatedSource
      |""".stripMargin.trim + "\n"
    )
  }
  implicit final class SourceOps(val source: Source) extends AnyVal {
    import source._

    def lineToOffset(line: LineNumber): Index   = lineIndices(line.index)
    def offsetToLine(offset: Index): LineNumber = coordinatesOf(offset).line
    def offsetToColumn(offset: Index): Int      = coordinatesOf(offset).column

    def charAt(offset: Index): Char      = chars(offset.value)
    def lineAt(line: LineNumber): String = lines(line.index)
    def lineNumbers                      = lines.indices map (x => LineNumber(x + 1))
  }
}
