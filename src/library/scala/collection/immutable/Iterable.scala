/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package immutable

import generic._

/** A base trait for iterable collections that are guaranteed immutable.
 *  $iterableInfo
 *
 *  @define Coll `immutable.Iterable`
 *  @define coll immutable iterable collection
 */
trait Iterable[+A] extends scala.collection.Iterable[A]
                      with GenericIterableTemplate[A, immutable.Iterable]
                      with IterableLike[A, immutable.Iterable[A]]
                      with Immutable
{
  override def companion: GenericCompanion[immutable.Iterable] = immutable.Iterable
}

/** $factoryInfo
 *  @define Coll `immutable.Iterable`
 *  @define coll immutable iterable collection
 */
object Iterable extends IterableFactory[immutable.Iterable] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Iterable[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: mutable.Builder[A, immutable.Iterable[A]] = new mutable.ListBuffer
}
