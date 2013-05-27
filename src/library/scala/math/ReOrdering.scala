package scala
package math

import java.{ lang => jl }
import Compared.{ LT, EQ, GT }

trait ReOrdering[@specialized(Boolean, Int, Long, Float, Double) A] extends Serializable {
  def compare(x: A, y: A): Compared
}

object ReOrdering extends ReOrderingImplicitOps {
  def apply[T](implicit ord: ReOrdering[T]) = ord

  def empty[A] : ReOrdering[A]                                = comparing[A]((_, _) => EQ)
  def comparing[A](cmp: (A, A) => Compared): ReOrdering[A]    = new ReOrdering[A] { def compare(x: A, y: A) = cmp(x, y) }
  def less[A](cmp: (A, A) => Boolean): ReOrdering[A]          = comparing[A]((x, y) => if (cmp(x, y)) LT else if (x == y) EQ else GT)
  def ordering[A](implicit ord: ReOrdering[A]): ReOrdering[A] = ord

  implicit val Boolean    = comparing[scala.Boolean]((x, y) => if (x == y) EQ else if (x) GT else LT)
  implicit val String     = comparing[jl.String]((x, y) => Compared(x compareTo y))
  implicit val Int        = comparing[scala.Int](Compared(_, _))
  implicit val Long       = comparing[scala.Long](Compared(_, _))
  implicit val Float      = comparing[scala.Float](Compared(_, _))
  implicit val Double     = comparing[scala.Double](Compared(_, _))
  implicit val BigInt     = comparing[scala.BigInt]((x, y) => Compared(x compare y))
  implicit val BigDecimal = comparing[scala.BigDecimal]((x, y) => Compared(x compare y))

  implicit def optionOrdering[A: ReOrdering, CC[X] <: Option[X]] : ReOrdering[CC[A]]          = empty[CC[A]] |> (_.toList)
  implicit def collectionOrdering[A: ReOrdering, CC[X] <: Traversable[X]] : ReOrdering[CC[A]] = new CollectionReOrdering[A, CC]

  import scala.math.{ ReOrdering => O }

  implicit def Tuple2[A1: O, A2: O]                      : O[(A1, A2)]             = empty[(A1, A2)]             |> (_._1) |> (_._2)
  implicit def Tuple3[A1: O, A2: O, A3: O]               : O[(A1, A2, A3)]         = empty[(A1, A2, A3)]         |> (x => (x._1, x._2) -> x._3)
  implicit def Tuple4[A1: O, A2: O, A3: O, A4: O]        : O[(A1, A2, A3, A4)]     = empty[(A1, A2, A3, A4)]     |> (x => (x._1, x._2, x._3) -> x._4)
  implicit def Tuple5[A1: O, A2: O, A3: O, A4: O, A5: O] : O[(A1, A2, A3, A4, A5)] = empty[(A1, A2, A3, A4, A5)] |> (x => (x._1, x._2, x._3, x._4) -> x._5)
}
