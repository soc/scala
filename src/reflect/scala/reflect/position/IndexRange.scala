package scala
package reflect
package position

import BitOps._

final class IndexRange private (val bits: Long) extends AnyVal {
  def start: Index = Index(left32(bits))
  def end: Index   = Index(right32(bits))
  def length: Int  = end.value - start.value

  def contains(that: Index): Boolean          = start <= that && that < end
  def union(that: IndexRange): IndexRange     = IndexRange(start min that.start, end max that.end)
  def intersect(that: IndexRange): IndexRange = IndexRange(start max that.start, end min that.end)

  override def toString = s"[$start,$end)"
}

object BitOps {
  final val Int4  = mask(4)
  final val Int12 = mask(12)
  final val Int14 = mask(14)
  final val Int16 = mask(16)
  final val Int18 = mask(18)
  final val Int20 = mask(20)
  final val Int24 = mask(24)

  final val Int32  = 0xFFFFFFFF
  final val Long32 = 0xFFFFFFFFL

  def mask(bits: Int): Int = (1 << bits) - 1

  def join64(lbits: Int, rbits: Int): Long = ((lbits & Long32) << 32) | (rbits & Long32)
  def left32(joined: Long): Int            = (joined >>> 32).toInt & Int32
  def right32(joined: Long): Int           = joined.toInt

  def join32(lbits: Int, rbits: Int): Int  = ((lbits & Int16) << 16) | (rbits & Int16)
  def left16(joined: Int): Int             = (joined >>> 16) & Int16
  def right16(joined: Int): Int            = joined & Int16

  def joinIntoInt(lsize: Int, lbits: Int, rbits: Int): Int = {
    assert(1 <= lsize && lsize <= 32, lsize)

    val rsize = 32 - lsize
    val lmask = mask(lsize)
    val rmask = mask(rsize)

    ((lbits & lmask) << rsize) | (rbits & rmask)
  }

  def leftN(lsize: Int)(lvalue: Int): Int  = lvalue >>> (32 - lsize)
  def rightN(rsize: Int)(rvalue: Int): Int = rvalue & mask(rsize)
}

object IndexRange {
  def apply(start: Index, end: Index): IndexRange = new IndexRange(join64(start.value, end.value))
}
