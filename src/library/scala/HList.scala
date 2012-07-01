package scala

import HList._

sealed trait HList {
  type Wrap[M[_]] <: HList
  def size: Int
}
sealed trait HNil extends HList {
  type Wrap[M[_]] = HNil
  def :+: [G](g: G): G :+: HNil = HCons(g, this)
  def size = 0

  override def toString = "HNil"
}
object HNil extends HNil { }

sealed case class HCons[H, T <: HList](head : H, tail : T) extends HList {
  type Wrap[M[_]] = M[H] :+: T#Wrap[M]
  def :+: [G](g: G): G :+: H :+: T = HCons(g, this)

  def size = 1 + tail.size
  override def toString = head + " :+: " + tail.toString
}

object HList {
  val :+: = HCons
  type :+:[H, T <: HList] = HCons[H,T]

  sealed trait TupleHListOps extends Any {
    def toHList: HList
    // Apparently can't implement this here without losing type info
    // def :+: [G](g: G)
  }
  implicit final class Tuple2HListOps[T1, T2](val x: (T1, T2)) extends AnyVal with TupleHListOps {
    def toHList = x._1 :+: x._2 :+: HNil
    def :+: [G](g: G) = HCons(g, toHList)
    def map[R](f: (T1, T2) => R) = f(x._1, x._2)
    def toList = x._1 :: x._2 :: Nil
  }
  implicit final class Tuple3HListOps[T1, T2, T3](val x: (T1, T2, T3)) extends AnyVal with TupleHListOps {
    def toHList = x._1 :+: x._2 :+: x._3 :+: HNil
    def :+: [G](g: G) = HCons(g, toHList)
    def map[R](f: (T1, T2, T3) => R) = f(x._1, x._2, x._3)
    def toList = List(x._1, x._2, x._3)
  }
  implicit final class Tuple4HListOps[T1, T2, T3, T4](val x: (T1, T2, T3, T4)) extends AnyVal with TupleHListOps {
    def toHList = x._1 :+: x._2 :+: x._3 :+: x._4 :+: HNil
    def :+: [G](g: G) = HCons(g, toHList)
    def map[R](f: (T1, T2, T3, T4) => R) = f(x._1, x._2, x._3, x._4)
    def toList = List(x._1, x._2, x._3, x._4)
  }
  implicit final class Tuple5HListOps[T1, T2, T3, T4, T5](val x: (T1, T2, T3, T4, T5)) extends AnyVal with TupleHListOps {
    def toHList = x._1 :+: x._2 :+: x._3 :+: x._4 :+: x._5 :+: HNil
    def :+: [G](g: G) = HCons(g, toHList)
    def map[R](f: (T1, T2, T3, T4, T5) => R) = f(x._1, x._2, x._3, x._4, x._5)
    def toList = List(x._1, x._2, x._3, x._4, x._5)
  }

  implicit def fromList[A](list: Traversable[A]) = ((HNil: HList) /: list) ( (hl,v) => HCons(v, hl) )



}
