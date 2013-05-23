package scala
package coll

trait Linear[+A, +Repr] extends CovariantContainer[A, Repr] {
  def head: A
  def tail: Repr
}

object Linear {
  def hasCheapSize = false
}
