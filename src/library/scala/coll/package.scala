package scala

package object coll {
  type tailrec = scala.annotation.tailrec
  type switch  = scala.annotation.switch
  type uV      = scala.annotation.unchecked.uncheckedVariance

  /** Operations which need to know how to deconstruct. */
  final implicit class LinearOps[A, Repr <: Linear[A, Repr]](val xs: Repr with Linear[A, Repr]) extends AnyVal {
    def nonEmpty = !xs.isEmpty
    def take(n: Int): Repr = {
      def loop(xs: Repr, n: Int): Repr = (
        if (n <= 0 || xs.isEmpty) xs
        else loop(xs.tail, n - 1)
      )
      loop(xs, n)
    }
    def drop(n: Int): Repr = {
      def loop(xs: Repr, n: Int): Repr = (
        if (n <= 0 || xs.isEmpty) xs
        else loop(xs.tail, n - 1)
      )
      loop(xs, n)
    }
    def exists(p: A => Boolean): Boolean = {
      def loop(xs: Repr): Boolean = (
        if (xs.isEmpty) false
        else p(xs.head) || loop(xs.tail)
      )
      loop(xs)
    }
    def contains(elem: A): Boolean = exists(_ == elem)
  }

  /** Operations which need to know how to construct. */
  final implicit class ListOps[A](val xs: List[A]) extends AnyVal {
    def ::(elem: A): List[A] = new ::[A](elem, xs)

    def filter(p: A => Boolean): List[A] = {
      def loop(xs: List[A]): List[A] = xs match {
        case Nil             => Nil
        case x :: xs if p(x) => x :: loop(xs)
        case _ :: xs         => loop(xs)
      }
      loop(xs)
    }

    def map[B](f: A => B): List[B] = {
      def loop(in: List[A], out: List[B]): List[B] = in match {
        case Nil     => out.reverse
        case x :: xs => loop(xs, f(x) :: out)
      }
      loop(xs, Nil)
    }

    def reverse: List[A] = {
      var in: List[A]  = xs
      var out: List[A] = Nil
      while (!in.isEmpty) {
        out ::= in.head
        in = in.tail
      }
      out
    }
  }
}
