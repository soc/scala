package scala
package coll

trait Container[+Repr] {
  def isEmpty: Boolean
}

trait InvariantContainer[A, +Repr] extends Foreach[A] { }
trait CovariantContainer[+A, +Repr] extends Foreach[A] { }

trait ContainerIsRepr[+Repr] extends ContainerHasRepr[Repr] {
  this: Repr =>

  def repr: Repr = this
}

trait ContainerHasRepr[+Repr] extends Container[Repr] {
  def repr: Repr
}

trait ContainerSemantics {
  def hasKnownSize: Boolean
  def hasCheapSize: Boolean
  def hasLazyElements: Boolean
  def hasUnderlyingContainer: Boolean
}
