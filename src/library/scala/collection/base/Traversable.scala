package scala
package collection
package base

trait Traversable[+A] {
  type M[+X]

  def foreach(f: A => Any): Unit
  def filter(f: A => Boolean): M[A]
  def map[B](f: A => B): M[B]
  def flatMap[B](f: A => M[B]): M[B]
  def collect[B](pf: PartialFunction[A, B]): M[B]
}

trait Map[A, +B] extends Traversable[(A, B)] {
  def apply(x: A): B
  def contains(x: A): Boolean
}

trait Set[+A] extends Traversable[A] {
  def apply(x)
}

trait Function1[-T, +R] {
  def apply(x: T): R
}

trait IndexedSeq[+A] extends Traversable[A] {
  def apply(x: Int): A
  def length: Int
}

trait LinearSeq[+A] extends Traversable[A] {
  def isEmpty: Boolean
  def head: A
  def tail: M[A]
}

object Traversable {
  implicit def indexedImplementations[A](xs: IndexedSeq[A]) = new collection.IndexedSeq[A] {

  }
  implicit def linearImplementations[A](xs: LinearSeq[A]) = new collection.LinearSeq[A] {

  }
}
