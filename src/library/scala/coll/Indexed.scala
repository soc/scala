package scala
package coll

trait Indexed[+A, +Repr] extends CovariantContainer[A, Repr] {
  def length: Int
  def apply(index: Int): A
}

object Indexed {
  def hasCheapSize = true
}

class StringSeq(val repr: String) extends Indexed[Char, StringSeq] with ContainerHasRepr[String] {
  def apply(index: Int): Char = repr charAt index
  def foreach(f: Char => Any) = {
    var i = 0
    while (i < length) {
      f(repr charAt i)
      i += 1
    }
  }
  def length  = repr.length
  def isEmpty = repr == ""
  override def toString = s"StringSeq($repr)"
}

final class ImmutableArraySeq[+A] private (private[this] val arr: Array[A]) extends Indexed[A, ImmutableArraySeq[A]] {
  def apply(index: Int): A = arr(index)
  def foreach(f: A => Any) = {
    var i = 0
    while (i < length) {
      f(arr(i))
      i += 1
    }
  }
  def size    = length
  def length  = arr.length
  def isEmpty = length == 0
  override def toString = arr.mkString("ImmutableArraySeq(", ", ", ")")
}

object ImmutableArraySeq {
  def apply[A](xs: A*): ImmutableArraySeq[A] = {
    val len = xs.size
    val arr = new Array[AnyRef](len)
    var i = 0
    xs foreach { x =>
      arr(i) = x.asInstanceOf[AnyRef]
      i += 1
    }
    new ImmutableArraySeq(arr.asInstanceOf[Array[A]])
  }
}
