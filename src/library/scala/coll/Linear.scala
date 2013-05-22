package scala
package coll

trait Linear[+A, +Repr] extends Foreach[A] {
  def isEmpty: Boolean
  def head: A
  def tail: Repr
}
