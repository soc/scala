/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala
package reflect.internal.util

/** new for position ranges */
class RangePosition private[util] (source: SourceFile, override val start: Int, point: Int, override val end: Int) extends OffsetPosition(source, point) {
  if (start > end) scala.sys.error("bad position: "+show)
  override def isRange: Boolean = true
  override def isOpaqueRange: Boolean = true
  override def startOrPoint: Int = start
  override def endOrPoint: Int = end
  override def withStart(off: Int) = Position.range(source, off, point, end)
  override def withEnd(off: Int) = Position.range(source, start, point, off)
  override def withPoint(off: Int) = Position.range(source, start, off, end)
  override def withSource(source: SourceFile, shift: Int) = Position.range(source, start + shift, point + shift, end + shift)
  override def focusStart = Position.offset(source, start)
  override def focus = {
    if (focusCache eq NoPosition) focusCache = Position.offset(source, point)
    focusCache
  }
  override def focusEnd = Position.offset(source, end)
  override def makeTransparent = Position.transparent(source, start, point, end)
  override def includes(pos: Position) = pos.isDefined && start <= pos.startOrPoint && pos.endOrPoint <= end
  override def union(pos: Position): Position =
    if (pos.isRange) Position.range(source, start min pos.start, point, end max pos.end) else this

  override def toSingleLine: Position = source match {
    case bs: BatchSourceFile
    if end > 0 && bs.offsetToLine(start) < bs.offsetToLine(end - 1) =>
      val pointLine = bs.offsetToLine(point)
      Position.range(source, bs.lineToOffset(pointLine), point, bs.lineToOffset(pointLine + 1))
    case _ => this
  }

  override def toString = "RangePosition("+source.file.canonicalPath+", "+start+", "+point+", "+end+")"
  override def show = "["+start+":"+end+"]"
  private var focusCache: Position = NoPosition
}

class TransparentPosition private[util] (source: SourceFile, start: Int, point: Int, end: Int) extends RangePosition(source, start, point, end) {
  override def isOpaqueRange: Boolean = false
  override def isTransparent = true
  override def makeTransparent = this
  override def show = "<"+start+":"+end+">"
}
