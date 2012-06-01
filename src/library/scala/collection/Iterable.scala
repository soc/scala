/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

import generic._
import scala.util.control.Breaks

/** A base trait for iterable collections.
 *  $iterableInfo
 */
trait Iterable[+A] extends IterableLike[A, Iterable[A]] with GenericIterableTemplate[A, Iterable] {
  override def companion: GenericCompanion[Iterable] = Iterable
}

/** $factoryInfo
 *  The current default implementation of a $Coll is a `Vector`.
 *  @define coll iterable collection
 *  @define Coll `Iterable`
 */
object Iterable extends IterableFactory[Iterable] {
  /** Provides break functionality separate from client code */
  private[collection] val breaks: Breaks = new Breaks

  /** $genericCanBuildFromInfo */
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Iterable[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]

  def newBuilder[A]: mutable.Builder[A, Iterable[A]] = immutable.Iterable.newBuilder[A]
  
  def fromForeach[A](fn: (A => Unit) => Unit): Iterable[A] = new AbstractIterable[A] {
    // override def foreach[U](f: A => Unit): Unit = fn(f)
    override def iterator = toBuffer.iterator
  }
}

/** Explicit instantiation of the `Iterable` trait to reduce class file size in subclasses. */
abstract class AbstractIterable[+A] extends Iterable[A]
