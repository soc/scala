package scala
package collection
package base

import scala.{ collection => coll }

package object cc {
  type Companion[+CC[X]] = CollectionCompanion { type M[+X] = CC[X] }
}

abstract class Dispatcher {
  @inline final def traversableImpls[A](xs: Traversable[A]): TraversableImpls[A] = new TraversableImpls(xs)
  @inline final def linearSeqImpls[A](xs: LinearSeq[A]): LinearSeqImpls[A]       = new LinearSeqImpls(xs)
  @inline final def indexedSeqImpls[A](xs: IndexedSeq[A]): IndexedSeqImpls[A]    = new IndexedSeqImpls(xs)
}

trait TraversableCore[+A] { self =>
  type M[+X] <: base.Traversable[A]
  type Companion <: CollectionCompanion { type M[+X] = self.M[X] }

  def companion: Companion
  def foreach(f: A => Any): Unit
  def filter(f: A => Boolean): M[A]
  def flatMap[B](f: A => M[B]): M[B]
  def collect[B](pf: PartialFunction[A, B]): M[B]
}

trait TraversableMethods[+A] extends Any {
  type M[+X] <: base.Traversable[X]
  type Companion <: cc.CollectionCompanion[M]

  def empty: M[A]
  def companion: Companion
  def slice(start: Int, end: Int): M[A]
  def drop(num: Int): M[A]
  def take(num: Int): M[A]
}
trait LinearSeqMethods[+A] extends Any with TraversableMethods[A] {
  type M[+X] <: base.LinearSeq[A]
}
trait IndexedSeqMethods[+A] extends Any with TraversableMethods[A] {
  type M[+X] <: base.IndexedSeq[A]
}

final class TraversableImpls[+A](val xs: base.Traversable[A]) extends AnyVal with TraversableMethods[A] {
  type M[+X] = base.Traversable[X]
  @inline final def empty = Nil
  @inline final def companion = TraversableImpls
  @inline final def drop(num: Int) = ???
  @inline final def take(num: Int) = ???
  @inline final def slice(from: Int, until: Int) = ???
}
object TraversableImpls extends CollectionCompanion {
  type M[+X] = base.Traversable[X]

  def apply[A](xs: A*): M[A]
  def empty[A] : M[A]

}

final class LinearSeqImpls[+A](val xs: base.LinearSeq[A]) extends AnyVal with LinearSeqMethods[A] {
  type M[+X] = base.LinearSeq[X]

  @inline final def drop(count: Int) = {
    var these = xs
    var count = n
    while (!these.isEmpty && count > 0) {
      these = these.tail
      count -= 1
    }
    these
  }
  @inline final def take(num: Int) = {
    val b = newBuilder
    var i = 0
    var these = repr
    while (!these.isEmpty && i < n) {
      i += 1
      b += these.head
      these = these.tail
    }
    b.result

  }
  @inline final def slice(from: Int, until: Int): base.LinearSeq[A] = {
    val start = from max 0
    val count = until - start
    if (until <= start) empty
    else xs drop start take count
  }
}
final class IndexedSeqImpls[+A](val xs: base.IndexedSeq[A]) extends AnyVal with TraversableMethods[A] {
  type M[+X] = base.IndexedSeq[X]
  @inline final def slice(from: Int, until: Int): base.IndexedSeq[A] = {
    val start = from max 0
    if (until <= start) empty
    else {
      val buf = new ArrayBuffer[A](until - start)
      var i   = start
      while (i < until) {
        buf(i) = xs(i)
        i += 1
      }
      buf.toIndexedSeq
    }
  }
}

// trait Functor extends Any {
//   type F[+X]

//   def map[A, B](f: A => B): F[A] => F[B]
// }
// trait Pointed[+A] extends Any with Functor[A] {
//   def pure(x: => A): F[A]
// }
// trait Monad[+A] extends Any with Applicative[A] with Pointed[A] {
//   def flatMap[B](f: A => F[B]): F[B]
// }
// trait MonadPlus[+A] extends Any with Monad[A] {
// }
// trait MonadZero[+A] extends Any with Monad[A] {
//   def filter[A](f: A => Boolean): F[A]
// }

trait CollectionCompanion {
  type M[+X] <: base.Traversable[X]

  def apply[A](xs: A*): M[A]
  def empty[A] : M[A]
}

trait MutableSeq[A] {
  def update(index: Int, value: A): Unit
}
trait MutableMap[A, B] {
  def update(index: A, value: B): Unit
}

trait Map[A, +B] extends Traversable[(A, B)] {
  def apply(x: A): B
  def contains(x: A): Boolean
}

trait Set[+A] extends Traversable[A] {
  def apply[A1 >: A](x: A1): Boolean
  def size: Int
}

trait Function1[-T, +R] {
  def apply(x: T): R
}

trait IndexedSeq[+A] extends Traversable[A] {
  type Companion <: CollectionCompanion { type M[+X] <: IndexedSeq[X] }
  def apply(x: Int): A
  def length: Int
}

trait LinearSeq[+A] extends Traversable[A] {
  type Companion <: CollectionCompanion { type M[+X] <: LinearSeq[X] }
  type M[+X] = LinearSeq[X]

  def isEmpty: Boolean
  def head: A
  def tail: M[A]
}

// object Traversable {
//   implicit def traversableImpls[A](xs: IndexedSeq[A]) = new collection.IndexedSeq[A] {

//   implicit def indexedImplementations[A](xs: IndexedSeq[A]) = new collection.IndexedSeq[A] {
//     def apply(idx: Int): A = xs(idx)
//     def length: Int = xs.length
//   }
//   implicit def linearImplementations[A](xs: LinearSeq[A]):collection.LinearSeq[A] = new collection.LinearSeq[A] {
//     def apply(idx: Int): A = ???
//     def length: Int = ???
//     override def isEmpty = xs.isEmpty
//     override def head = xs.head
//     override def tail = xs.tail
//   }
// }
