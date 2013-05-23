package scala
package coll

trait Show[-A] {
  def showAll(xs: Foreach[A]): String
  def showOne(x: A): String
}

object Show {
  object ToString extends Show[Any] {
    def showAll(xs: Foreach[Any]): String = xs map showOne mkString ", "
    def showOne(x: Any): String = "" + x
  }
}
