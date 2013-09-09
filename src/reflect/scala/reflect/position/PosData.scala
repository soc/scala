package scala
package reflect
package position

import PosData._

final class PosData private (val bits: Long) extends AnyVal {
  def start: Int                 = (bits >>> StartShift).toInt & MaxValue
  def point: Int                 = (bits >>> PointShift).toInt & MaxValue
  def end: Int                   = (bits >>> EndShift).toInt & MaxValue
  def width: Int                 = end - start
  def endOrElse(alt: Int): Int   = if (isRange) end else alt
  def startOrElse(alt: Int): Int = if (isRange) start else alt

  def isDefined: Boolean                      = this != NoPosData
  def isFocused: Boolean                      = (start == point) && (point == end)
  def isRange: Boolean                        = isDefined && (start < end)
  def isTransparent: Boolean                  = (bits & TransparentBit) != 0
  def includes(pos: PosData): Boolean         = start <= pos.start && pos.end <= end
  def properlyIncludes(pos: PosData): Boolean = start < pos.start && pos.end < end
  def precedes(pos: PosData): Boolean         = end <= pos.start
  def properlyPrecedes(pos: PosData): Boolean = end < pos.start
  def overlaps(pos: PosData): Boolean         = (
       (start < pos.start) && (pos.end < end)
    || (pos.start < start) && (end < pos.end)
  )
  def sameRange(pos: PosData): Boolean = (start == pos.start) && (end == pos.end)

  def focus: PosData                 = if (isFocused) this else PosData.focused(point)
  def focusStart: PosData            = PosData.focused(start)
  def focusEnd: PosData              = PosData.focused(end)
  def makeTransparent: PosData       = if (isTransparent) this else new PosData(bits | TransparentBit)
  def union(pos: PosData): PosData   = PosData(start min pos.start, point, end max pos.end)
  def withStart(start: Int): PosData = PosData(start, point, end, isTransparent)
  def withPoint(point: Int): PosData = PosData(start, point, end, isTransparent)
  def withEnd(end: Int): PosData     = PosData(start, point, end, isTransparent)
  def shifted(shift: Int): PosData   = PosData(start + shift, point + shift, end + shift, isTransparent)

  def copy(start: Int = start, point: Int = point, end: Int = end): PosData = PosData(start, point, end)

  private def ldelim    = if (isTransparent) "<" else "["
  private def rdelim    = if (isTransparent) ">" else "]"
  override def toString = s"$ldelim$start:$end${rdelim}^$point"
}

object PosData {
  final val None        = apply(0, 0, 0, false)
  final val FakePosData = apply(0, 0, 0, true)

  final val BitsPerInt     = 20
  final val MaxValue       = (1L << BitsPerInt).toInt - 1
  final val StartShift     = 63 - BitsPerInt
  final val PointShift     = 63 - (BitsPerInt * 2)
  final val EndShift       = 63 - (BitsPerInt * 3)
  final val TransparentBit = 1L << 0

  def validate(start: Int, point: Int, end: Int): Unit = {
    def params = ((start, point, end))

    require(start <= MaxValue && point <= MaxValue && end <= MaxValue, s"All parameters are limited to $MaxValue: " + params)
    require(start >= 0 && point >= 0 && end >= 0, s"All parameters must be non-negative: " + params)
    require(start <= end, s"Start must not be greater than end: " + params)
  }

  def focused(point: Int): PosData                           = create(point, point, point, isTransparent = false)
  def opaque(start: Int, point: Int, end: Int): PosData      = create(start, point, end, isTransparent = false)
  def transparent(start: Int, point: Int, end: Int): PosData = create(start, point, end, isTransparent = true)

  def apply(pos: UtilPosition): PosData = (
    if (pos.isRange) apply(pos.start, pos.point, pos.end)
    else if (pos.isDefined) apply(pos.point)
    else NoPosData
  )
  def apply(point: Int): PosData                                               = apply(point, point, point)
  def apply(start: Int, end: Int): PosData                                     = apply(start, start, end)
  def apply(start: Int, point: Int, end: Int): PosData                         = apply(start, point, end, isTransparent = false)
  def apply(start: Int, point: Int, end: Int, isTransparent: Boolean): PosData = create(start, point, end, isTransparent)

  private def create(start: Int, point: Int, end: Int, isTransparent: Boolean): PosData = {
    validate(start, point, end)
    val trans = if (isTransparent) TransparentBit else 0L
    new PosData(start.toLong << StartShift | point.toLong << PointShift | end.toLong << EndShift | trans)
  }
}
