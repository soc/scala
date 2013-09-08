package scala
package reflect
package position

import scala.reflect.io.PlainFile

trait Pos {

}

class CompatPosition[T](val positioned: Positioned[api.Position]) extends ApiPosition {
  type Pos = api.Position { type Pos <: api.Position }

  def withPos(pos: Pos) = ???
  // def includes(pos: Pos): Boolean         = positioned
  // def properlyIncludes(pos: Pos): Boolean = ???
  // def precedes(pos: Pos): Boolean         = ???
  // def properlyPrecedes(pos: Pos): Boolean = ???
  // def overlaps(pos: Pos): Boolean         = ???
  // def sameRange(pos: Pos): Boolean        = ???
  // def union(pos: Pos): Pos                = ???

  def includes(pos: Pos)                  = pos.isDefined && start <= pos.startOrPoint && pos.endOrPoint <= end
  def union(pos: Pos): Pos                = if (pos.isRange) newRange(start = start min pos.start, end = end max pos.end) else this
  def properlyIncludes(pos: Pos): Boolean = includes(pos) && (start < pos.startOrPoint || pos.endOrPoint < end)
  def precedes(pos: Pos): Boolean         = isDefined && pos.isDefined && endOrPoint <= pos.startOrPoint
  def properlyPrecedes(pos: Pos): Boolean = isDefined && pos.isDefined && endOrPoint < pos.startOrPoint
  def overlaps(pos: Pos): Boolean         = (
       isRange
    && pos.isRange
    && ((pos.start < end && start < pos.end) || (start < pos.end && pos.start < end))
  )
  def sameRange(pos: Pos): Boolean = isRange && pos.isRange && start == pos.start && end == pos.end

  private def newRange(start: Int = start, point: Int = point, end: Int = end): Pos =
    new RangePosition(source, start, point, end)

  def column: Int                = positioned.column
  def end: Int                   = positioned.end
  def endOrPoint: Int            = positioned endOrElse point
  def focus: Pos                 = newRange(point, point, point)
  def focusEnd: Pos              = newRange(end, end, end)
  def focusStart: Pos            = newRange(start, start, start)
  def isDefined: Boolean         = positioned.isDefined
  def isOpaqueRange: Boolean     = isRange
  def isRange: Boolean           = isDefined && start != end
  def isTransparent: Boolean     = false
  def line: Int                  = positioned.line
  def lineContent: String        = positioned.lineContent
  def makeTransparent: Pos       = pos
  def point: Int                 = positioned.point
  def pointOrElse(alt: Int): Int = positioned pointOrElse alt
  def pos: api.Position          = newRange()
  def show: String               = positioned.show
  def source: SourceFile         = new BatchSourceFile(new PlainFile(positioned.source.file))
  def start: Int                 = positioned.start
  def startOrPoint: Int          = positioned startOrElse point
  def toSingleLine: Pos          = ???
  def withEnd(end: Int): Pos     = newRange(end = end)
  def withPoint(point: Int): Pos = newRange(point = point)
  def withStart(start: Int): Pos = newRange(start = start)
}

trait ApiPosition extends api.Position {
  self =>

  type Pos >: Null <: api.Position
  type SelfType = macros.Attachments { type Pos = self.Pos }

  def pos: Pos
  // def withPos(newPos: Pos): SelfType
  def all: Set[Any]
  def get[T: ClassTag]: Option[T]
  def update[T: ClassTag](attachment: T): SelfType
  def remove[T: ClassTag]: SelfType

  def source: scala.reflect.internal.util.SourceFile
  def isDefined: Boolean
  def isRange: Boolean
  def isTransparent: Boolean
  def isOpaqueRange: Boolean
  def makeTransparent: Pos
  def start: Int
  def startOrPoint: Int
  def point: Int
  def pointOrElse(default: Int): Int
  def end: Int
  def endOrPoint: Int
  def withStart(off: Int): Pos
  def withEnd(off: Int): Pos
  def withPoint(off: Int): Pos
  def union(pos: Pos): Pos
  def focus: Pos
  def focusStart: Pos
  def focusEnd: Pos
  def includes(pos: Pos): Boolean
  def properlyIncludes(pos: Pos): Boolean
  def precedes(pos: Pos): Boolean
  def properlyPrecedes(pos: Pos): Boolean
  def overlaps(pos: Pos): Boolean
  def sameRange(pos: Pos): Boolean
  def line: Int
  def column: Int
  def toSingleLine: Pos
  def lineContent: String
  def show: String
}
