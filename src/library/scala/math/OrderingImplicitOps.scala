package scala
package math

import java.util.Comparator
import Compared.{ LT, EQ, GT }
import Ordering._

trait PackageLevelOrderingOps {
  implicit def orderingContravariance[A, A1 <: A](ord: Ordering[A]): Ordering[A1] =
    ord.asInstanceOf[Ordering[A1]]

  implicit class OrderingElementOps[A: Ordering](lhs: A) {
    def cmp(rhs: A): Compared = implicitly[Ordering[A]].compare(lhs, rhs)
    def <(rhs: A)  = cmp(rhs).isLess
    def <=(rhs: A) = cmp(rhs).isLessOrEqual
    def >(rhs: A)  = cmp(rhs).isGreater
    def >=(rhs: A) = cmp(rhs).isGreaterOrEqual

    def equiv(rhs: A)  = cmp(rhs).isEqual
    def max(rhs: A): A = if (this < rhs) rhs else lhs
    def min(rhs: A): A = if (this > rhs) rhs else lhs
  }
}

trait OrderingImplicitOps {
  implicit def oldOrderingToOrdering[A](ord: scala.math.old.Ordering[A]): Ordering[A] =
    new Ordering[A] { def compare(x: A, y: A): Int = ord.compare(x, y).value }

  implicit def comparablesToOrdering[A](implicit cmp: A => Comparable[A]): Ordering[A] =
    comparing[A]((x, y) => Compared(x compareTo y))

  implicit def comparatorToOrdering[A, Coll <: Comparator[A]](cmp: Coll): Ordering[A] =
    comparing[A]((x, y) => Compared(cmp.compare(x, y)))

  implicit class OrderingOps[A](ord: Ordering[A]) {
    def on[B](f: B => A): Ordering[B] = map(f)

    def map[B](f: B => A): Ordering[B] =
      comparing[B]((x, y) => ord.compare(f(x), f(y)))

    def orElse[B: Ordering](f: A => B): Ordering[A] =
      comparing[A]((x, y) => ord.compare(x, y) || ordering[B].map(f).compare(x, y))

    def reverse: Ordering[A] =
      comparing[A]((x, y) => ord.compare(x, y).flip)

    def |> [B: Ordering](f: A => B): Ordering[A] = orElse[B](f)

    def toComparator[A1 <: A]: Comparator[A1] =
      (new Comparator[A] { def compare(x: A, y: A) = ord.compare(x, y).value }).asInstanceOf[Comparator[A1]]
  }

  class CollectionOrdering[A : Ordering, CC[X] <: Traversable[X]] extends Ordering[CC[A]] {
    def compare(x: CC[A], y: CC[A]): Compared = {
      var result: Compared = EQ
      val xs = x.toIterator
      val ys = y.toIterator
      while (xs.hasNext && ys.hasNext && result.isEqual)
        result = xs.next cmp ys.next

      if (result.isUnequal) result
      else if (xs.hasNext == ys.hasNext) EQ
      else if (ys.hasNext) LT
      else GT
    }
  }
}
