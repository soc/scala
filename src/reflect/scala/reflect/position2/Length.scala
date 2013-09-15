package scala
package reflect
package position

final class Length[T] private (val value: Int) extends AnyVal {
  def isEmpty: Boolean  = value == 0
  def length: Int       = value
  override def toString = "" + length
}

object Length {
  def apply[T](length: Int): Length[T] =
    if (length < 0) throw new IllegalArgumentException(s"length: $length")
    else new Length[T](length)
}
