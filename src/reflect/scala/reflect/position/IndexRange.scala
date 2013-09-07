package scala
package reflect
package position

import JoinInts._

final class IndexRange private (val bits: Long) extends AnyVal {
  def start: Index = Index(left(bits))
  def end: Index   = Index(right(bits))
  def length: Int  = end.value - start.value

  def contains(that: Index): Boolean          = start <= that && that < end
  def union(that: IndexRange): IndexRange     = IndexRange(start min that.start, end max that.end)
  def intersect(that: IndexRange): IndexRange = IndexRange(start max that.start, end min that.end)

  override def toString = s"[$start,$end)"
}

object JoinInts {
  final val Int32   = 0xFFFFFFFF
  final val Long32  = 0xFFFFFFFFL

  def join32(lbits: Int, rbits: Int): Long = ((lbits & Long32) << 32) | (rbits & Long32)
  def left(joined: Long): Int              = (joined >>> 32).toInt & Int32
  def right(joined: Long): Int             = joined.toInt
}

object JoinUnsignedShorts {
  final val Int16 = 0x0000FFFF

  def join16(lbits: Int, rbits: Int): Int = ((lbits & Int16) << 32) | (rbits & Int16)
  def left(joined: Int): Int              = (joined >>> 16) & Int16
  def right(joined: Int): Int             = joined & Int16
}

object IndexRange {
  def apply(start: Index, end: Index): IndexRange = new IndexRange(join32(start.value, end.value))
}
