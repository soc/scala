package scala
package coll

sealed trait List[+A] extends Linear[A, List[A]] with ContainerIsRepr[List[A]] {
  def foreach(f: A => Any) = List.foreach(this, f)
  override def toString = s"PNil" // only so I don't get confused what List I'm looking at
}

final case class ::[+A](head: A, tail: List[A]) extends List[A] {
  def isEmpty = false

  override def toString = s"$head :: $tail"
}

final case object Nil extends List[Nothing] {
  def isEmpty = true
  def head    = sys.error("Nil.head")
  def tail    = sys.error("Nil.tail")
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
