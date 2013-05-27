package scala
package math

import Compared._

/** Compared is a value class encoding the three possible
  * terminating outcomes of an inequality test: left side
  * is { LT, EQ, GT } the right.
  *
  * @author Paul Phillips
  * @since 2.11
  */
final class Compared private (val value: Byte) extends AnyVal {
  def isLess           = this == LT
  def isGreater        = this == GT
  def isEqual          = this == EQ
  def isUnequal        = this != EQ
  def isLessOrEqual    = this != GT
  def isGreaterOrEqual = this != LT
  def flip             = if (this == LT) GT else if (this == GT) LT else EQ

  @inline def || (op: => Compared): Compared = if (isUnequal) this else op

  override def toString = this match {
    case LT => "LT"
    case EQ => "EQ"
    case GT => "GT"
  }
}

object Compared {
  import ReOrdering._

  def apply(x: Long, y: Long): Compared     = apply(x - y)
  def apply(x: Int, y: Int): Compared       = apply(x - y)
  def apply(x: Float, y: Float): Compared   = apply(java.lang.Float.compare(x, y))
  def apply(x: Double, y: Double): Compared = apply(java.lang.Double.compare(x, y))

  def apply[T: ReOrdering](lhs: T, rhs: T): Compared = lhs cmp rhs //implicitly[ReOrdering[T]].compare(lhs, rhs)

  def apply(difference: Long): Compared = (
    if (difference < 0) LT
    else if (difference > 0) GT
    else EQ
  )
  def apply(difference: Int): Compared = (
    if (difference < 0) LT
    else if (difference > 0) GT
    else EQ
  )

  final val LT = new Compared(-1)
  final val EQ = new Compared(0)
  final val GT = new Compared(1)
}
