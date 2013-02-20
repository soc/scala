package s

class HMap[K, +V]()

trait Fun1[-T, +R] {
  def apply(v1: T): R
  def compose[A](g: A => T): A => R = { x => apply(g(x)) }
  def andThen[A](g: R => A): T => A = { x => g(apply(x)) }
}
case class Tup2[+T1, T2](x1: T1, x2: T2)
abstract class AbFun1[-T, +R] extends Fun1[T, R] { }
