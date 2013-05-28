package scala
package math

import java.{ lang => jl }
import Compared.{ LT, EQ, GT }

trait Ordering[@specialized(Boolean, Int, Long, Float, Double) A] extends Serializable {
  def compare(x: A, y: A): Compared
}

object Ordering extends OrderingImplicitOps {
  // def apply[T](implicit ord: Ordering[T]) = ord

  def empty[A] : Ordering[A]                                = comparing[A]((_, _) => EQ)
  def comparing[A](cmp: (A, A) => Compared): Ordering[A]    = new Ordering[A] { def compare(x: A, y: A) = cmp(x, y) }
  def less[A](cmp: (A, A) => Boolean): Ordering[A]          = comparing[A]((x, y) => if (cmp(x, y)) LT else if (x == y) EQ else GT)
  def ordering[A](implicit ord: Ordering[A]): Ordering[A] = ord

  // compat
  def fromLessThan[T](cmp: (T, T) => Boolean) = less(cmp)

  implicit val Boolean    = comparing[scala.Boolean]((x, y) => if (x == y) EQ else if (x) GT else LT)
  implicit val String     = comparing[jl.String]((x, y) => Compared(x compareTo y))
  implicit val Int        = comparing[scala.Int](Compared(_, _))
  implicit val Long       = comparing[scala.Long](Compared(_, _))
  implicit val Float      = comparing[scala.Float](Compared(_, _))
  implicit val Double     = comparing[scala.Double](Compared(_, _))
  implicit val BigInt     = comparing[scala.BigInt]((x, y) => Compared(x compare y))
  implicit val BigDecimal = comparing[scala.BigDecimal]((x, y) => Compared(x compare y))

  implicit def optionOrdering[A: Ordering, CC[X] <: Option[X]] : Ordering[CC[A]]          = empty[CC[A]] |> (_.toList)
  implicit def collectionOrdering[A: Ordering, CC[X] <: Traversable[X]] : Ordering[CC[A]] = new CollectionOrdering[A, CC]

  import scala.math.{ Ordering => O }

  implicit def Tuple2[A1: O, A2: O]                      : O[(A1, A2)]             = empty[(A1, A2)]             |> (_._1) |> (_._2)
  implicit def Tuple3[A1: O, A2: O, A3: O]               : O[(A1, A2, A3)]         = empty[(A1, A2, A3)]         |> (x => (x._1, x._2) -> x._3)
  implicit def Tuple4[A1: O, A2: O, A3: O, A4: O]        : O[(A1, A2, A3, A4)]     = empty[(A1, A2, A3, A4)]     |> (x => (x._1, x._2, x._3) -> x._4)
  implicit def Tuple5[A1: O, A2: O, A3: O, A4: O, A5: O] : O[(A1, A2, A3, A4, A5)] = empty[(A1, A2, A3, A4, A5)] |> (x => (x._1, x._2, x._3, x._4) -> x._5)
}
