package scala
package math

import java.util.Comparator
import Compared.{ LT, EQ, GT }
import ReOrdering._

trait PackageLevelOrderingOps {
  implicit class ReOrderingElementOps[A: ReOrdering](lhs: A) {
    def cmp(rhs: A): Compared = implicitly[ReOrdering[A]].compare(lhs, rhs)
    def <(rhs: A)  = cmp(rhs).isLess
    def <=(rhs: A) = cmp(rhs).isLessOrEqual
    def >(rhs: A)  = cmp(rhs).isGreater
    def >=(rhs: A) = cmp(rhs).isGreaterOrEqual

    def equiv(rhs: A)  = cmp(rhs).isEqual
    def max(rhs: A): A = if (this < rhs) rhs else lhs
    def min(rhs: A): A = if (this > rhs) rhs else lhs
  }
}

trait ReOrderingImplicitOps {
  import ReOrdering._

  implicit def reorderingToOrdering[A](ord: ReOrdering[A]): Ordering[A] =
    new Ordering[A] { def compare(x: A, y: A): Int = ord.compare(x, y).value }

  implicit def reorderingContravariance[A, A1 <: A](ord: ReOrdering[A]): ReOrdering[A1] =
    ord.asInstanceOf[ReOrdering[A1]]

  implicit def comparablesToReOrdering[A](implicit cmp: A => Comparable[A]): ReOrdering[A] =
    comparing[A]((x, y) => Compared(x compareTo y))

  implicit def comparatorToReOrdering[A, Coll <: Comparator[A]](cmp: Coll): ReOrdering[A] =
    comparing[A]((x, y) => Compared(cmp.compare(x, y)))

  implicit class ReOrderingOps[A](ord: ReOrdering[A]) {
    def map[B](f: B => A): ReOrdering[B] =
      comparing[B]((x, y) => ord.compare(f(x), f(y)))

    def orElse[B: ReOrdering](f: A => B): ReOrdering[A] =
      comparing[A]((x, y) => ord.compare(x, y) || ordering[B].map(f).compare(x, y))

    def reverse: ReOrdering[A] =
      comparing[A]((x, y) => ord.compare(x, y).flip)

    def |> [B: ReOrdering](f: A => B): ReOrdering[A] = orElse[B](f)
  }

  class CollectionReOrdering[A : ReOrdering, CC[X] <: Traversable[X]] extends ReOrdering[CC[A]] {
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
