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

/** A base trait for iterable collections that can be mutated.
 *  $iterableInfo
 */
trait Iterable[A] extends scala.collection.Iterable[A]
                     with GenericIterableTemplate[A, mutable.Iterable]
                     with IterableLike[A, mutable.Iterable[A]]
                     with Mutable
{
  override def companion: GenericCompanion[mutable.Iterable] = mutable.Iterable
}

/** $factoryInfo
 *  The current default implementation of a $Coll is an `ArrayBuffer`.
 *  @define coll mutable iterable collection
 *  @define Coll `mutable.Iterable`
 */
object Iterable extends IterableFactory[mutable.Iterable] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, mutable.Iterable[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, mutable.Iterable[A]] = new ArrayBuffer
}

/** Explicit instantiation of the `Iterable` trait to reduce class file size in subclasses. */
abstract class AbstractIterable[A] extends scala.collection.AbstractIterable[A] with mutable.Iterable[A]
