package scala
package reflect
package position

import scala.reflect.internal.util._
import BitOps._

final class SourceId private (val bits: Short) extends AnyVal with Ordered[SourceId] {
  def id: Int = bits
  def compare(that: SourceId): Int = id compare that.id
  override def toString = s"$id"
}
final class NodeId private (val bits: Int) extends AnyVal with Ordered[NodeId] {
  def id = bits
  def compare(that: NodeId): Int = id compare that.id
  override def toString = s"$id"
}

final class TreeId private (val bits: Int) extends AnyVal with Ordered[TreeId] {
  def compare(that: TreeId): Int = sourceId compare that.sourceId match {
    case 0 => nodeId compare that.nodeId
    case n => n
  }
  def sourceId: SourceId = SourceId(leftN(12)(bits))
  def nodeId: NodeId     = NodeId(rightN(20)(bits))

  override def toString = s"$sourceId/$nodeId"
}

object SourceId extends (Int => SourceId) {
  final val Max = Int12
  val None = new SourceId(0)
  def apply(id: Int): SourceId = new SourceId(boundsCheck(0, Max)(id).toShort)
}
object NodeId extends (Int => NodeId) {
  final val Max = Int20
  val None = new NodeId(0)
  def apply(id: Int): NodeId = new NodeId(boundsCheck(0, Max)(id))
}
object TreeId extends ((SourceId, NodeId) => TreeId) {
  val None = new TreeId(0)
  def apply(sourceId: SourceId, nodeId: NodeId): TreeId = (
    if (nodeId == NoNodeId) None
    else new TreeId(joinIntoInt(lsize = 14, sourceId.id, nodeId.id))
  )
}

trait TextView extends Any {
  def asCharArray: Array[Char]
  def asLines: Vector[String]
  def asString: String
}

trait Source extends Any {
  def uri: Uri
  def chars: IndexedSeq[Char]
  def lines: IndexedSeq[String]
  def lineIndices: IndexedSeq[Index]
  def coordinatesOf(offset: Index): RowAndColumn
}

object NoSource extends Source {
  val uri                          = newUri("")
  val chars                        = Vector()
  val lines                        = Vector()
  val lineIndices                  = Vector()
  def coordinatesOf(offset: Index) = RowAndColumn.Empty
}

class UriSource(val uri: Uri, val chars: ImmutableChars) extends Source {
  val lines       = chars.lines
  val lineIndices = chars.lineIndices

  if (this.file.length > 0 && (sys.props contains "dump")) this.dump()

  def coordinatesOf(offset: Index): RowAndColumn = lineIndices indexWhere offset.< match {
    case -1    => RowAndColumn.Empty
    case index => RowAndColumn(index, (lineIndices(index - 1) to offset).length)
  }
  override def toString = uri.toString
}

object Source {
  private def charsOf(arr: Array[Char]) = if (arr eq null) ImmutableChars(Array[Char]()) else ImmutableChars(arr)

  def apply(uri: java.net.URI): Source  = new UriSource(uri, ImmutableChars(uri))
  def apply(file: java.io.File): Source = if (file eq null) NoSource else apply(file.toURI)
  def apply(file: SourceFile): Source   = if (file.file.file eq null) NoSource else new UriSource(file.file.file.toURI, charsOf(file.content))
  def apply(path: String): Source       = apply(newUri(path))

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
      |${source.path}
      |   chars: ${chars.length}
      |   lines: ${lines.length}
      |  source:
      |$annotatedSource
      |""".stripMargin.trim + "\n"
    )
  }
  implicit final class SourceOps(val source: Source) extends AnyVal {
    import source._

    def url  = uri.toURL
    def file = new File(uri)
    def name = file.getName
    def path = file.getPath

    def lineToOffset(line: LineNumber): Index   = lineIndices(line.index)
    def offsetToLine(offset: Index): LineNumber = coordinatesOf(offset).line
    def offsetToColumn(offset: Index): Int      = coordinatesOf(offset).column

    def charAt(offset: Index): Char      = chars(offset.value)
    def lineAt(line: LineNumber): String = lines(line.index)
    def lineNumbers                      = lines.indices map (x => LineNumber(x + 1))
  }
}
