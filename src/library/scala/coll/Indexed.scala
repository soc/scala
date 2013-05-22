package scala
package coll

trait Indexed[+A] extends Foreach[A] {
  def length: Int
  def apply(index: Int): A
}
