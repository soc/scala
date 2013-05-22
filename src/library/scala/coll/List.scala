package scala
package coll

sealed trait List[+A] extends Linear[A, List[A]] {
  def foreach(f: A => Any) = List.foreach(this, f)
}

final case class ::[+A](head: A, tail: List[A]) extends List[A] {
  def isEmpty = false
  def repr    = this
}

final case object Nil extends List[Nothing] {
  def isEmpty = true
  def head    = sys.error("Nil.head")
  def tail    = sys.error("Nil.tail")
  def repr    = this
}

object List {
  def foreach[A](xs: List[A], f: A => Any): Unit = {
    def loop(xs: List[A]) {
      if (xs.nonEmpty) {
        f(xs.head)
        loop(xs.tail)
      }
    }
    loop(xs)
  }
  def apply[A](xs: A*): List[A] = {
    var buf: List[A] = Nil
    xs foreach (x => buf = ::(x, buf))
    buf.reverse
  }
}
