package scala
package reflect
package position

import scala.reflect.internal.util._

abstract class CompatPositioner[T <: api.Position] extends Positioner[T] {
  def sourceFor(pos: T): Source
  def positioned(pos: T): Positioned[T]

  private implicit def liftIndex(index: Int): Index = Index(index)

  def reposition(pos: T, range: IndexRange): T = (new RangePosition(pos.source, range.start.value, pos.point, range.end.value)).asInstanceOf[T]
  def rangeFor(pos: T): IndexRange             = (
    if (pos.isRange) IndexRange(pos.start, pos.end)
    else if (pos.isDefined) pos.point.selfRange
    else 0.emptyRange
  )
  def pointFor(pos: T): Index      = pos.point
  def isDefinedAt(pos: T): Boolean = pos.isDefined
  def undefined: T                 = NoPosition.asInstanceOf[T]
}

object CompatPositioner extends CompatPositioner[api.Position] {
  def sourceFor(pos: api.Position): Source                    = Source(pos.source)
  def positioned(pos: api.Position): Positioned[api.Position] = new CompatPositioned(this, pos)

  def apply[T <: api.Position] : CompatPositioner[T] = this.asInstanceOf[CompatPositioner[T]]
}

final class CompatPositioned[T <: api.Position](val positioner: Positioner[T], val pos: T) extends AnyRef with Positioned[T] {
  def source: Source            = positioner sourceFor pos
  def coordinates: RowAndColumn = source coordinatesOf caratPoint
  def range: IndexRange         = positioner rangeFor pos
  def caratPoint: Index         = positioner pointFor pos
  def isDefined: Boolean        = positioner isDefinedAt pos

  def line: Int                  = if (isDefined && !coordinates.isEmpty) coordinates.row else -1
  def column: Int                = if (isDefined && !coordinates.isEmpty) coordinates.column else -1
  def start: Int                 = range.start.value
  def point: Int                 = caratPoint.value
  def end: Int                   = range.end.value
  def startOrElse(alt: Int): Int = if (isDefined) start else alt
  def pointOrElse(alt: Int): Int = if (isDefined) point else alt
  def endOrElse(alt: Int): Int   = if (isDefined) end else alt
  def lineContent: String        = source lineAt LineNumber(line)
  def lineCarat: String          = mapToWhitespace(lineContent take column - 1) + "^"
  def show                       = pos.show

  def coordinatesString = "${source.name}:$line:$column"
  def formattedMessage(prefix: String, msg: String): String = s"$prefix:$line: $msg\n$lineContent\n$lineCarat"
}
