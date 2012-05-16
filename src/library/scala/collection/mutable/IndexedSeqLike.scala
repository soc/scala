/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package mutable
import generic._

/** A subtrait of scala.collection.IndexedSeq which represents sequences
 *  that can be mutated.
 *
 *  It declares a method `update` which allows updating an element
 *  at a specific index in the sequence.
 *
 *  This trait just implements `iterator` in terms of `apply` and `length`.
 *  However, see `IndexedSeqOptimized` for an implementation trait that overrides operations
 *  to make them run faster under the assumption of fast random access with `apply`.
 *
 *  $indexedSeqInfo
 *
 *  @tparam A    the element type of the $coll
 *  @tparam Repr the type of the actual $coll containing the elements.
 *
 *  @define Coll `IndexedSeq`
 *  @define coll mutable indexed sequence
 *  @define indexedSeqInfo
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8
 *  @define willNotTerminateInf
 *  @define mayNotTerminateInf
 */
trait IndexedSeqLike[A, +Repr] extends scala.collection.IndexedSeqLike[A, Repr] { self =>

  override protected[this] def thisCollection: IndexedSeq[A] = this.asInstanceOf[IndexedSeq[A]]
  override protected[this] def toCollection(repr: Repr): IndexedSeq[A] = repr.asInstanceOf[IndexedSeq[A]]

  /** Replaces element at given index with a new value.
   *
   *  @param idx     the index of the element to replace.
   *  @param elem    the new value.
   *  @throws   IndexOutOfBoundsException if the index is not valid.
   */
  def update(idx: Int, elem: A)
}
