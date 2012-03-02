/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package util

import java.lang.ref.SoftReference
import collection.mutable.{ Buffer, ArrayBuffer }

/** A wrapper which acts like a Buffer[T] but uses a Buffer[SoftReference[T]]
 *  under the covers.  So those Ts can turn into nulls at any time.  Don't use
 *  this unless that's what you want.  It throws a custom exception if you try
 *  to access a garbage-collected element via apply, but iterator and foreach
 *  will give you the nulls so the length is right.
 */
class SoftBuffer[T >: Null](private val elems: Buffer[SoftReference[T]]) extends Buffer[T] {
  def this() = this(ArrayBuffer[SoftReference[T]]())
  private def wrap(x: T)     = new SoftReference(x)
  private def fail(idx: Int) = throw new SoftBuffer.CollectedElement(idx)

  def apply(idx: Int): T = elems(idx).get match {
    case null   => fail(idx)
    case x      => x
  }
  def update(idx: Int, elem: T): Unit = elems(idx) = wrap(elem)
  def length = elems.length
  def iterator = elems.iterator map (_.get)
  def += (elem: T): this.type = {
    elems += wrap(elem)
    this
  }
  def clear() = elems.clear()
  def +=:(elem: T): this.type = {
    wrap(elem) +=: elems
    this
  }
  def insertAll(n: Int, xs: collection.Traversable[T]) = elems.insertAll(n, xs map wrap)
  def remove(n: Int) = elems.remove(n).get
  def uncollected = elems map (_.get) filterNot (_ == null)
  def isUncollected(idx: Int) = elems(idx).get != null

  override def toString() = "SoftReference wrapper: %d / %d uncollected".format(uncollected.length, length)
}

object SoftBuffer {
  class CollectedElement(index: Int) extends RuntimeException("element has been collected: " + index) { }
}
