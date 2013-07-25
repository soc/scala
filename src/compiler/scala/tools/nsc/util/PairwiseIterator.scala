/* NSC -- new Scala compiler
 * Copyright 2006-2012 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package util

import scala.collection.GenTraversableOnce

/** A structure for iterating over pairs of elements.
 *  This exists primarily because an Iterator[(A, A)] involves
 *  creating a tuple for every pair.  If hasNext is true,
 *  both left and right should contain valid values of A.
 *  If the iterator is exhausted, advance() should do nothing.
 */
trait PairwiseIterator[+A] {
  def left: A
  def right: A
  def isEmpty: Boolean
  def advance(): Unit
}

object PairwiseIterator {
  def sliding[A](xs: GenTraversableOnce[A]): PairwiseIterator[A] =
    new SlidingPairwiseImpl(xs.toIterator)
  def apply[A](lefts: GenTraversableOnce[A])(rights: A => GenTraversableOnce[A]): PairwiseIterator[A] =
    new PairwiseIteratorImpl(lefts.toIterator, rights andThen (_.toIterator))

  // If you really need a regular iterator, ok.
  private class ScalaIteratorWrapper[A](it: PairwiseIterator[A]) extends Iterator[(A, A)] {
    def hasNext = it.hasNext
    def next()  = ((it.left, it.right))
  }
  private class MappedPairwiseIterator[A, B](it: PairwiseIterator[A], f: (A, A) => B) extends Iterator[B] {
    def hasNext = it.hasNext
    def next    = try f(it.left, it.right) finally it.advance()
  }

  implicit class PairwiseIteratorOps[A](val it: PairwiseIterator[A]) extends AnyVal {
    outer =>

    private def next() = try both finally it.advance()
    def toScalaIterator: Iterator[(A, A)] = new ScalaIteratorWrapper(it)
    def hasNext = !it.isEmpty
    def both: (A, A) = ((it.left, it.right))
    def map[B](f: (A, A) => B): Iterator[B] = new MappedPairwiseIterator[A, B](it, f)
    def foreach(f: (A, A) => Any) {
      while (hasNext) {
        f(it.left, it.right)
        it.advance()
      }
    }
  }
}

class SlidingPairwiseImpl[+A](it: Iterator[A]) extends PairwiseIterator[A] {
  private[this] var _left: A  = _
  private[this] var _right: A = _
  private[this] var _isEmpty = it.isEmpty || {
    _left = it.next()
    it.isEmpty || {
      _right = it.next()
      false
    }
  }
  def left: A  = _left
  def right: A = _right
  def isEmpty  = _isEmpty
  def advance() {
    if (it.hasNext) {
      _left = _right
      _right = it.next()
    }
    else _isEmpty = true
  }
}

class PairwiseIteratorImpl[+A](lefts: Iterator[A], rights: A => Iterator[A]) extends PairwiseIterator[A] {
  private[this] var _left: A  = _
  private[this] var _right: A = _
  private[this] var rightIterator: Iterator[A] = _
  private[this] var _hasNext = advanceLeft()

  private def advanceRight(): Boolean = (
    rightIterator.hasNext && {
      _right = rightIterator.next()
      true
    }
  )
  private def advanceLeft(): Boolean = (
    lefts.hasNext && {
      _left = lefts.next
      rightIterator = rights(_left)
      advanceRight() || advanceLeft()
    }
  )

  def left    = _left
  def right   = _right
  def isEmpty = !hasNext
  def hasNext = _hasNext
  def advance() {
    advanceRight() || advanceLeft() || { _hasNext = false ; false }
  }
}
