package scala
package reflect
package position

import JoinInts._

final class IndexRange[T] private (val bits: Long) extends AnyVal {
  def start: Index[T]   = Index(left(bits))
  def end: Index[T]     = Index(right(bits))
  def length: Length[T] = Length[T](end.value - start.value)

  def contains(that: Index[T]): Boolean             = start <= that && that < end
  def union(that: IndexRange[T]): IndexRange[T]     = IndexRange.exclusive(start min that.start, end max that.end)
  def intersect(that: IndexRange[T]): IndexRange[T] = IndexRange.exclusive(start max that.start, end min that.end)

  override def toString = s"[${start.index}, ${end.index})"
}

object JoinInts {
  final val Int32   = 0xFFFFFFFF
  final val Long32  = 0xFFFFFFFFL

  private def join(lbits: Long, rbits: Long)     = (lbits << 32) | rbits

  def join(lbits: Int, rbits: Int): Long = join(lbits & Long32, rbits & Long32)
  def left(joined: Long): Int            = (joined >>> 32).toInt & Int32
  def right(joined: Long): Int           = joined.toInt
}

object IndexRange {
  def exclusive[T](start: Index[T], end: Index[T]): IndexRange[T] = new IndexRange[T](join(start.value, end.value))
  def inclusive[T](start: Index[T], end: Index[T]): IndexRange[T] = exclusive(start, end.next)
  def steps[T](start: Index[T], length: Length[T]): IndexRange[T] = (
    if (length.isEmpty) start.selfRange
    else exclusive(start, start add length.length)
  )
}
