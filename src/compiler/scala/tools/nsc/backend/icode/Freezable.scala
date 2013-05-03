/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package backend
package icode

import scala.collection.{ mutable, immutable }
import mutable.ListBuffer

trait Freezable[A] {
  def isFrozen: Boolean
  def freeze(): Frozen[A]
  def thaw[A1 >: A](): Thawed[A1]
}
trait Thawed[A] extends Freezable[A] {
  def foreach[U](f: A => U): Unit
  def update(idx: Int, elem: A): Unit
  def append(elem: A): this.type
  def prepend(elem: A): this.type
  def transform(f: A => A): this.type
  def retain(p: A => Boolean): this.type
}
trait Frozen[A] extends Freezable[A] {
  // def thaw(): Thawed[A]
}

trait FreezableSeq[A] extends collection.Seq[A] with Freezable[A] {
  def freeze(): FrozenSeq[A]
  // def thaw(): ThawedSeq[A]
  def thaw[A1 >: A](): ThawedSeq[A1]
}

class ThawedSeq[A: ClassManifest] extends mutable.Seq[A] with FreezableSeq[A] with Thawed[A] {
  private[this] var frozen = false
  private val buf = ListBuffer[A]()
  @inline private[this] def andThis(body: => Any): this.type = {
    assert(!isFrozen, this)
    body
    this
  }
  def isFrozen = frozen
  override def foreach[U](f: A => U): Unit = buf.toList foreach f
  def apply(idx: Int)      = buf(idx)
  def iterator             = buf.iterator
  def length               = buf.length

  def update(idx: Int, elem: A): Unit = andThis(buf(idx) = elem)
  def append(elem: A): this.type = andThis(buf += elem)
  def appendAll(elems: TraversableOnce[A]): this.type = andThis(buf ++= elems)
  def prepend(elem: A): this.type = andThis(buf prepend elem)
  override def transform(f: A => A): this.type = andThis(buf transform f)
  def retain(p: A => Boolean): this.type = {
    andThis(buf.indices.reverse filter (idx => p(buf(idx))) foreach (buf remove _))
  }
  def flatTransform(f: A => Seq[A]): this.type = {
    val xs = buf.toList
    buf.clear()
    buf ++= (xs flatMap f)
    this
  }
  // def freeze[B >: A](): FrozenSeq[A, B] = synchronized {
  def freeze(): FrozenSeq[A] = synchronized {
    assert(!isFrozen, this)
    frozen = true
    new FrozenSeq[A](this)
  }
  def unfreeze(): this.type = {
    assert(isFrozen, this)
    frozen = false
    this
  }
  def thaw[A1 >: A]() = this
  // def thaw(): ThawedSeq[A] = this
  def toArray: Array[A] = buf.toArray
}

class FrozenSeq[+A : ClassManifest](thawed: ThawedSeq[A]) extends immutable.Seq[A] with FreezableSeq[A] with Frozen[A] {
  private[this] val elems = thawed.toArray

  def isFrozen             = true
  def apply(idx: Int)      = elems(idx)
  def iterator             = elems.iterator
  def length               = elems.length
  def thaw[A1 >: A]() = thawed.unfreeze()
  def freeze() = this

  // def freeze[B >: A](): FrozenSeq[A, B] = new FrozenSeq[A, B](thawed)
}
