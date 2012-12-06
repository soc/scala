/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */
package scala.reflect.internal.util

final class PositionData private (val underlying: Long) extends AnyVal {
  import PositionData._

  private def ints: Long = underlying >>> 1
  private def intAt(which: Int) = (ints >>> (IntBits * which)).toInt & Mask21
  private def int1: Int  = (ints >>> (IntBits * 2)).toInt & Mask21
  private def int2: Int  = (ints >>> (IntBits * 1)).toInt & Mask21
  private def int3: Int  = (ints >>> (IntBits * 0)).toInt & Mask21
  private def tbit       = underlying.toInt & 1

  // private def lint           = (underlying >>> 32).toInt
  // private def rint           = (underlying & -1).toInt
  // private def pointOffset    = (rint >>> EndOffsetBits) & MaxPointOffset
  // private def endOffset      = (rint & MaxEndOffset).toInt
  // private def transparentBit = rint & Int.MinValue

  def withStart(start1: Int) = create(start1, point, end, isTransparent)
  def withPoint(point1: Int) = create(start, point1, end, isTransparent)
  def withEnd(end1: Int)     = create(start, point, end1, isTransparent)
  def shiftBy(amount: Int)   = create(start + amount, point + amount, end + amount, isTransparent)

  def isDefined     = this != NoPositionData
  def isRange       = start != point || end != point
  def isTransparent = isRange && tbit != 0
  def isOpaqueRange = isRange && tbit == 0
  def start         = int1
  def point         = int2
  def end           = int3
  // def lo            = start min point
  // def hi            = end max point
  // def start         = lint
  // def point         = start + pointOffset
  // def end           = start + endOffset

  def makeTransparent             = if (isTransparent) this else new PositionData(underlying | 1L)
  def includes(pos: PositionData) = (
    start <= pos.start && pos.end <= end
  )
  //      (start <= pos.startOrPoint && pos.endOrPoint <= end)
  //      // (start <= pos.start && pos.end <= end)
  //   // || (lo <= pos.lo && pos.hi <= hi)
  // )
// ((pos.start < end && start < pos.end) || (start < pos.end && pos.start < end))

  def overlaps(pos: PositionData) = (
       (pos.start < end && start < pos.end)
    || (start < pos.end && pos.start < end)
  )
  // def includes(pos: PositionData) = pos.isDefined && start <= pos.start && pos.end <= end
  def union(pos: PositionData)    = {
    val res = PositionData(start min pos.start, point, end max pos.end)
    // log("union", s"$this U $pos == $res")
    res
  }
  def show                        = s"[$start:$end]"

  // def widthIncludingPoint = hi - lo
  def widthOfRange        = end - start
  // def widthBeforeStart    = start - lo
  // def widthAfterEnd       = hi - end

  def isInRange(idx: Int) = start <= idx && idx <= end

  private def inRed(value: Any) = value.toString match {
    case "" => ""
    case s  => Console.RED + Console.BOLD + s + Console.RESET
  }
  def codeString(source: SourceFile): String =
    if (source == null || source.content == null) toString
    else codeString(source.content.slice(start, end).mkString)

  def codeString(code: String): String = {
    val pre  = "" //inRed(code take widthBeforeStart)
    val mid  = code // code.substring(widthBeforeStart, widthBeforeStart + widthOfRange)
    val post = "" // inRed(code takeRight widthAfterEnd)
    val lena = ""
    // val lena = if (start == lo && end == hi) "" else s"${inRed(widthBeforeStart)}/$widthOfRange/${inRed(widthAfterEnd)}"

    "[%8d:%-4d%s] %s".format(start, widthOfRange, lena, trunc(code))
  }

  override def toString = {
    if (underlying == -1L) "NoPositionData"
    else if (!isRange) "" + point
    else {
      val point_s = if (start == point) "" else "/" + point
      val tran_s  = if (isTransparent) "/transparent" else ""
      s"$start-$end$point_s$tran_s"
    }
  }
}

object PositionData {
  final val IntBits = 21
  final val Mask21  = (1 << IntBits) - 1

  var currentSource: SourceFile = NoSourceFile

  final val MaxAbsolutePos  = Mask21
  final val NoPositionData  = new PositionData(-1L)
  final val PointOffsetBits = 11
  final val EndOffsetBits   = 20
  final val MaxPointOffset  = (1 << PointOffsetBits) - 1
  final val MaxEndOffset    = (1 << EndOffsetBits) - 1
  // final val TransparentMask = Int.MaxValue.toLong + 1

  def trunc(s: String): String = s.lines.toList mkString " [NL] "

  def apply(source: SourceFile, start: Int, point: Int, end: Int): PositionData = {
    currentSource = source
    apply(start, point, end)
  }
  def apply(start: Int, point: Int, end: Int): PositionData =
    create(start max 0 min point, point, end max point, isTransparent = false)

  def transparent(start: Int, point: Int, end: Int): PositionData =
    create(start, point, end, isTransparent = true)

  private def log(label: String, msg: => Any) {
    // Console.err.println("[%6s] %s".format(label, msg))
     // s"[ fail ] $msg ${pdata.codeString(currentSource)}")
  }

  private def create(start: Int, point: Int, end: Int, isTransparent: Boolean): PositionData = {
    def transInput = if (isTransparent) ", isTransparent=true" else ""
    def inputs = s"($start, $point, $end$transInput)"
    def assemble(s: Int, p: Int, e: Int) = {
      val int1: Long = s.toLong << (IntBits * 2 + 1)
      val int2: Long = p.toLong << (IntBits * 1 + 1)
      val int3: Long = e.toLong << (IntBits * 0 + 1)
      val tbit: Long = if (isTransparent) 1L else 0L

      new PositionData(int1 | int2 | int3 | tbit)
    }
    def asserts(where: String)(s: Int, p: Int, e: Int) {
      def assert(cond: Boolean, msg: String) {
        if (!cond) {
          val pdata = assemble(s, p, e)
          val pstr  = if (currentSource eq NoSourceFile) "<no source>" else pdata.codeString(currentSource)

          log("fail", s"($where: $msg) $inputs => $pstr")
          // (new Throwable).getStackTrace take 15 drop 1 foreach println
        }
      }
      assert(s >= 0, "start >= 0")
      assert(s <= p, "start <= point")
      assert(p <= e, "point <= end")
      assert(s <= MaxAbsolutePos, s"start <= $MaxAbsolutePos")
      assert(p <= MaxAbsolutePos, s"point <= $MaxAbsolutePos")
      assert(e <= MaxAbsolutePos, s"end <= $MaxAbsolutePos")
    }

    // if (end == MaxAbsolutePos) {
    //   log("badend", s"$currentSource / $inputs")
    //   (new Throwable).getStackTrace take 25 drop 1 foreach println
    // }

    asserts("incoming")(start, point, end)

    // val start1 = (start max 0 min point) min MaxAbsolutePos
    // val start1 = start max 0 min MaxAbsolutePos
    val start1 = start min MaxAbsolutePos
    val point1 = point min MaxAbsolutePos
    val end1   = end min MaxAbsolutePos
    // val end1   = (end max point) min MaxAbsolutePos
    asserts("normalized")(start1, point1, end1)

    val res = assemble(start1, point1, end1)
    log("create", f"$inputs%40s => $res")
    res
  }
}
