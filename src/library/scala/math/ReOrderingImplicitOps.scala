package scala
package math

import java.util.Comparator
import Compared.{ LT, EQ, GT }
import ReOrdering._

trait PackageLevelOrderingOps {
  implicit class ReOrderingElementOps[A: ReOrdering](lhs: A) {
    def cmp(rhs: A): Compared = summon[A].compare(lhs, rhs)
    def <(rhs: A): Boolean  = cmp(rhs).isLess
    def <=(rhs: A): Boolean = cmp(rhs).isLessOrEqual
    def >(rhs: A): Boolean  = cmp(rhs).isGreater
    def >=(rhs: A): Boolean = cmp(rhs).isGreaterOrEqual

    def equiv(rhs: A): Boolean  = cmp(rhs).isEqual
    def max(rhs: A): A = if (this < rhs) rhs else lhs
    def min(rhs: A): A = if (this > rhs) rhs else lhs
  }
}

trait LowPriorityReOrderingImplicitOps {
  implicit def reorderingToOrdering[A](ord: ReOrdering[A]): scala.math.Ordering[A] = ord.toOldOrdering
}

trait ReOrderingImplicitOps extends LowPriorityReOrderingImplicitOps {
  implicit def reorderingContravariance[A, A1 <: A](ord: ReOrdering[A]): ReOrdering[A1] =
    ord.asInstanceOf[ReOrdering[A1]]

  implicit def comparablesToReOrdering[A](implicit cmp: A => Comparable[A]): ReOrdering[A] =
    comparing[A]((x, y) => Compared(x compareTo y))

  implicit def comparatorToReOrdering[A, Coll <: Comparator[A]](cmp: Coll): ReOrdering[A] =
    comparing[A]((x, y) => Compared(cmp.compare(x, y)))

  implicit class ReOrderingOps[A](ord: ReOrdering[A]) {
    //  XXX remove
    // def reverse = flip
    // def |> [B: ReOrdering](f: A => B) = this | (summon[B] map f).compare

    def | (cmp: (A, A) => Compared) = join[A](ord.compare, cmp)

    def cast[B] : ReOrdering[B] = ord.asInstanceOf[ReOrdering[B]]

    def map[B](f: B => A) = comparing[B]((x, y) => ord.compare(f(x), f(y)))

    def flip = comparing[A](ord.compare(_, _).flip)

    def ++ [B: ReOrdering](f: A => B) = this | (summon[B] map f).flip.compare

    def -- [B: ReOrdering](f: A => B) = this | (summon[B] map f).compare

    def toComparator: Comparator[A] =
      new Comparator[A] { def compare(x: A, y: A): Int = ord.compare(x, y).value }

    def toOldOrdering: Ordering[A] =
      new scala.math.Ordering[A] { def compare(x: A, y: A): Int = ord.compare(x, y).value }
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
