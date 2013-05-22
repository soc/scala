package scala
package coll

trait Foreach[+A] {
  def foreach(f: A => Any): Unit
}
