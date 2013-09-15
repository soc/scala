package scala
package reflect
package position

import PosData._
import BitOps._

// final class PosData private (val start: Int, val point: Int, val end: Int, val flags: Int) {//extends AnyVal {

final class PosData private (val bits: Long) extends AnyVal {
  def start: Int       = ((bits >>> 40) & 0xFFFFFFL).toInt
  def pointOffset: Int = ((bits >>> 28) & 0xFFFL).toInt
  def end: Int         = ((bits >>> 4) &  0xFFFFFFL).toInt
  def flags: Int       = (bits & 0xFL).toInt
  def width: Int       = end - start
  def point: Int       = if (hasNegativePoint) start - pointOffset else start + pointOffset

  def indexRange = Index(start) until Index(end)

  def startOrElse(alt: Int): Int = if (isRange) start else alt
  def pointOrElse(alt: Int): Int = if (isDefined) point else alt
  def endOrElse(alt: Int): Int   = if (isRange) end else alt
  def startOrPoint: Int          = startOrElse(point)
  def endOrPoint: Int            = endOrElse(point)

  def focus: PosData      = toOffset(point)
  def focusStart: PosData = toOffset(start)
  def focusEnd: PosData   = toOffset(end)

  def isDefined     = this != PosData.None
  def isUndefined   = this == PosData.None
  def isFocused     = isDefined && !hasRangeBit
  def isRange       = isDefined && hasRangeBit
  def isTransparent = isRange && hasTransparentBit
  def isOpaqueRange = isRange && !hasTransparentBit

  private def toOffset(point: Int) = PosData offset point
  private def hasNoPositionBit     = (flags & NoPositionBit) != 0
  private def hasNegativePoint     = (flags & NegativePointBit) != 0
  private def hasTransparentBit    = (flags & TransparentBit) != 0
  private def hasRangeBit          = (flags & RangeBit) != 0

  private def checkOverlap(p1: PosData, p2: PosData) = p2.start < p1.end && p1.start < p2.end

  def precedes(that: PosData)         = isDefined && that.isDefined && endOrPoint <= that.startOrPoint
  def properlyPrecedes(that: PosData) = isDefined && that.isDefined && endOrPoint <  that.startOrPoint
  def includes(that: PosData)         = isRange && start <= that.startOrPoint && that.endOrPoint <= end
  def properlyIncludes(that: PosData) = isRange && start <  that.startOrPoint && that.endOrPoint <  end
  def overlaps(that: PosData)         = isRange && that.isRange && (checkOverlap(this, that) || checkOverlap(that, this))
  def sameRange(that: PosData)        = isRange && that.isRange && start == that.start && end == that.end

  // override def makeTransparent = new TransparentPosition(source, start, point, end)
  // override def includes(pos: Position) = pos.isDefined && start <= pos.startOrPoint && pos.endOrPoint <= end
  // override def union(pos: Position): Position =
  //   if (pos.isRange) new RangePosition(source, start min pos.start, point, end max pos.end) else this

  def union(that: PosData): PosData = (
    if (isRange && that.isRange) PosData.range(start min that.start, point, end max that.end)
    else if (isRange) this
    else if (that.isRange) that
    else this
  )

  def makeTransparent = if (isTransparent) this else copy(flags = flags | TransparentBit)
  def copy(start: Int = start, point: Int = point, end: Int = end, flags: Int = flags): PosData = PosData.create(start, point, end, flags)

  def show = (
    if (isUndefined) "[NoPosition]" //"<nopos>"
    else if (isOpaqueRange) s"[$start:$end]"
    else if (isTransparent) s"<$start:$end>"
    else s"[$point]"
  )
    // else if (isOpaqueRange && start != point) s"[$point/$start:$end]"

  override def toString = show
}

object PosData {
  final val None = new PosData(NoPositionBit.toLong)

  // |------------------------|------------|------------------------|----|
  // | start: 24              | point: 12  | end: 24                |flag|
  // | 63 - 40                | 39 - 28    | 27 - 4                 |3-0 |

  final val StartBits = 24
  final val PointBits = 12
  final val EndBits   = 24
  final val FlagBits  = 4

  final val MaxStart         = Int24
  final val StartShift       = 40
  final val PointShift       = 28
  final val EndShift         = 4
  final val TransparentBit   = 1 << 3
  final val NoPositionBit    = 1 << 2
  final val NegativePointBit = 1 << 1
  final val RangeBit         = 1 << 0

  def validate(start: Int, point: Int, end: Int): Unit = {
    val pointOffset = point - start
    def params = ((start, point, end))

    require(start <= MaxStart && end <= MaxStart, s"start and end are limited to $MaxStart")
    require(start >= 0 && pointOffset >= 0 && end >= 0, s"All parameters must be non-negative: " + params)
    require(start <= end, s"Start must not be greater than end: " + params)
  }

  def apply(pos: ApiPosition): PosData = compat(pos)

  def compat(pos: api.Position): PosData = /*printResult(s"compat(${pos.show})")*/(
    if (pos.isOpaqueRange) range(pos.start, pos.point, pos.end)
    else if (pos.isTransparent) transparent(pos.start, pos.point, pos.end)
    else if (pos.isDefined) offset(pos.point)
    else None
  )

  def offset(point: Int): PosData                            = create(point, point, point, 0)
  def range(start: Int, point: Int, end: Int): PosData       = create(start, point, end, RangeBit)
  def transparent(start: Int, point: Int, end: Int): PosData = create(start, point, end, TransparentBit | RangeBit)

  private def printResult[T](msg: String)(body: T): T = { println(s"$msg: $body") ; body }
  private def create(start: Int, point: Int, end: Int, flags: Int): PosData = {
    // println(s"create($start, $point/$offset, $end, $flags)")

    // new PosData(start, point, end, flags)

    val offset      = math.abs(point - start)
    val bits1: Long = (start & 0xFFFFFFL) << 40
    val bits2: Long = (offset & 0xFFFL) << 28
    val bits3: Long = (end & 0xFFFFFFL) << 4
    val bits4: Long = (flags & 0xFL) | ( if (point < start) NegativePointBit else 0 )

    new PosData(bits1 | bits2 | bits3 | bits4)

    // // println(s"""|bits1=$bits1
    // //             |bits2=$bits2
    // //             |bits3=$bits3
    // //             |bits4=$bits4""".stripMargin)

    // printResult(s"create($start, $point (offset=$offset), $end, flags=$flags)")(
    // )
  }
}


  // def isTransparent: Boolean                  = (bits & TransparentBit) != 0
  // def includes(pos: PosData): Boolean         = start <= pos.start && pos.end <= end
  // def properlyIncludes(pos: PosData): Boolean = start < pos.start && pos.end < end
  // def precedes(pos: PosData): Boolean         = end <= pos.start
  // def properlyPrecedes(pos: PosData): Boolean = end < pos.start
  // def overlaps(pos: PosData): Boolean         = (
  //      (start < pos.start) && (pos.end < end)
  //   || (pos.start < start) && (end < pos.end)
  // )
  // def sameRange(pos: PosData): Boolean = (start == pos.start) && (end == pos.end)

  // def makeTransparent: PosData       = if (isTransparent) this else new PosData(bits | TransparentBit)
  // def union(pos: PosData): PosData   = PosData(start min pos.start, point, end max pos.end)
  // def withStart(start: Int): PosData = PosData(start, point, end, isTransparent)
  // def withPoint(point: Int): PosData = PosData(start, point, end, isTransparent)
  // def withEnd(end: Int): PosData     = PosData(start, point, end, isTransparent)

  // private def ldelim    = if (isTransparent) "<" else "["
  // private def rdelim    = if (isTransparent) ">" else "]"
  // override def toString = s"$ldelim$start:$end${rdelim}^$point"

  // /*** Compat ***/

  // def isRange         = true
  // def isOpaqueRange   = true
  // def startOrPoint    = start
  // def endOrPoint      = end
  // def focusStart      = this
  // def focus           = this
  // def focusEnd        = this
  // def show            = toString
  // def isDefined       = true
  // def pointOrElse(default: Int): Int = point

  // def source: Nothing        = ???
  // def line: Int              = ???
  // def safeLine: Int          = ???
  // def column: Int            = ???
  // def toSingleLine: PosData = ???
  // def lineContent: String    = ???
// }


// object RangePos {
//   implicit def rangePosToData(p: RangePos): PosData = p.data
//   implicit def posDataToRange(p: PosData): RangePos = apply(null, p)

//   def apply(source: SourceFile, data: PosData): RangePos                    = new RangePos(source, data)
//   def apply(source: SourceFile, start: Int, end: Int): RangePos             = apply(source, start, point = start, end)
//   def apply(source: SourceFile, start: Int, point: Int, end: Int): RangePos = new RangePos(source, PosData(start, point, end, isTransparent = false))
// }

// class RangePos private (val source: SourceFile, val data: PosData) extends scala.reflect.api.Position {
//   type Pos = scala.reflect.api.Position
//   def this(source: SourceFile, start: Int, point: Int, end: Int) = this(source, PosData(start, point, end))

//   def length = end - start
//   // override def sourceCode: String = new String(source.content, start, length)

//   override def start                                      = data.start
//   override def end                                        = data.end
//   override def isTransparent                              = data.isTransparent
//   override def isOpaqueRange: Boolean                     = !isTransparent
//   override def isRange: Boolean                           = true
//   override def startOrPoint: Int                          = start
//   override def endOrPoint: Int                            = end
//   override def withStart(off: Int)                        = RangePos(source, off, point, end)
//   override def withEnd(off: Int)                          = RangePos(source, start, point, off)
//   override def withPoint(off: Int)                        = RangePos(source, start, off, end)
//   // override def withSource(source: SourceFile, shift: Int) = RangePos(source, start + shift, point + shift, end + shift)
//   override def focusStart                                 = OffsetPos(source, start)
//   override def focus                                      = OffsetPos(source, point)
//   override def focusEnd                                   = OffsetPos(source, end)
//   override def makeTransparent                            = if (isTransparent) this else RangePos(source, data.makeTransparent)
//   override def includes(pos: Pos)                         = pos.isDefined && start <= pos.startOrPoint && pos.endOrPoint <= end
//   override def union(pos: Pos): Pos                       = if (pos.isRange) RangePos(source, start min pos.start, point, end max pos.end) else this

//   def pos: scala.reflect.api.Position = ???
//   def withPos(newPos: Position): Attachments{ type Pos = Position } = ???
//   def column: Int = ???
//   def isDefined: Boolean = ???
//   def line: Int = ???
//   def lineContent: String = ???
//   def overlaps(pos: scala.reflect.api.Position): Boolean = ???
//   def point: Int = ???
//   def pointOrElse(default: Int): Int = ???
//   def precedes(pos: scala.reflect.api.Position): Boolean = ???
//   def properlyIncludes(pos: scala.reflect.api.Position): Boolean = ???
//   def properlyPrecedes(pos: scala.reflect.api.Position): Boolean = ???
//   def sameRange(pos: scala.reflect.api.Position): Boolean = ???

//   override def toSingleLine: Pos = source match {
//     case bs: BatchSourceFile
//     if end > 0 && bs.offsetToLine(start) < bs.offsetToLine(end - 1) =>
//       val pointLine = bs.offsetToLine(point)
//       RangePos(source, bs.lineToOffset(pointLine), point, bs.lineToOffset(pointLine + 1))
//     case _ => this
//   }

//   override def toString = data.toString
//   override def show = "["+start+":"+end+"]"
// }

// class OffsetPos(val source: SourceFile, val offset: Int) extends scala.reflect.api.Position {
//   type Pos = scala.reflect.api.Position
//   def pos: Pos = ???
//   def withPos(newPos: Pos): Attachments { type Pos = OffsetPos.this.Pos } = ???

//   // Members declared in scala.reflect.api.Position
//   def column: Int = ???
//   def end: Int = ???
//   def endOrPoint: Int = ???
//   def focus: Pos = ???
//   def focusEnd: Pos = ???
//   def focusStart: Pos = ???
//   def includes(pos: Pos): Boolean = ???
//   def isDefined: Boolean = ???
//   def isOpaqueRange: Boolean = ???
//   def isRange: Boolean = ???
//   def isTransparent: Boolean = ???
//   def line: Int = ???
//   def lineContent: String = ???
//   def makeTransparent: Pos = ???
//   def overlaps(pos: Pos): Boolean = ???
//   def point: Int = ???
//   def pointOrElse(default: Int): Int = ???
//   def precedes(pos: Position): Boolean = ???
//   def properlyIncludes(pos: Position): Boolean = ???
//   def properlyPrecedes(pos: Position): Boolean = ???
//   def sameRange(pos: Position): Boolean = ???
//   def show: String = ???
//   def start: Int = ???
//   def startOrPoint: Int = ???
//   def toSingleLine: Pos = ???
//   def union(pos: Pos): Pos = ???
//   def withEnd(off: Int): Pos = ???
//   def withPoint(off: Int): Pos = ???
//   def withStart(off: Int): Pos = ???
// }

// object OffsetPos {
//   def apply(source: SourceFile, offset: Int): OffsetPos = new OffsetPos(source, offset)
// }
