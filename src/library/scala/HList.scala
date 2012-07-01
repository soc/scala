package scala

import HList._

sealed trait HList {
  type Wrap[M[_]] <: HList
}
sealed trait HNil extends HList {
  type Wrap[M[_]] = HNil
  def :+: [G](g: G): G :+: HNil = HCons(g, this)

  override def toString = "HNil"
}

object HNil extends HNil
final case class HCons[H, T <: HList](head : H, tail : T) extends HList {
  type Wrap[M[_]] = M[H] :+: T#Wrap[M]
  def :+: [G](g: G): G :+: H :+: T = HCons(g, this)

  override def toString = head + " :+: " + tail.toString
}

object HList {
  val :+: = HCons
  type :+:[H, T <: HList] = HCons[H,T]

  implicit def tuple5tohlist[T1, T2, T3, T4, T5](x: (T1, T2, T3, T4, T5)) =
  x._1 :+: x._2 :+: x._3 :+: x._4 :+: x._5 :+: HNil

  // contains no type information: not even A
  // implicit def fromList[A](list: Traversable[A]): HList = ((HNil: HList) /: list) ( (hl,v) => HCons(v, hl) )
}