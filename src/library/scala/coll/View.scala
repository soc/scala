package scala
package coll

/** TODO

trait View
trait ViewOps[A, Repr <: Container[A, Repr]] {
  sealed trait ViewOp
  case class ReprOp(f: Repr => Repr) extends ViewOp
  case class FilterOp[A](p: A => Boolean) extends ViewOp
  case class MapOp[B](f: A => B) extends ViewOp

  class View(repr: Repr, ops: List[ViewOp]) extends InvariantContainer[A, View] {
    def this(repr: Repr) = this(repr, Nil)

    def foreach(f: A => Any): Unit = repr foreach { x =>

    }
    def filter(p: A => Boolean): View = new View(FilterOp(p) :: ops)
    def map[B](f: A => B): View = new View(MapOp(f) :: ops)
  }
}

**/
