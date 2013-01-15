/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2010-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection.generic

import scala.collection.parallel.Combiner
import scala.collection.parallel.ParIterable
import scala.collection.parallel.ParMap
import scala.collection.parallel.TaskSupport
import scala.annotation.unchecked.uncheckedVariance
import scala.language.higherKinds

/** A template trait for collections having a companion.
 *
 *  @tparam A    the element type of the collection
 *  @tparam CC   the type constructor representing the collection class
 *  @author Aleksandar Prokopec
 *  @since 2.8
 */
trait GenericParTemplate[+A, +CC[X] <: ParIterable[X]]
extends GenericTraversableTemplate[A, CC] with HasNewCombiner[A, CC[A] @uncheckedVariance] {

  protected[this] override def newBuilder: scala.collection.mutable.Builder[A, CC[A]] = newCombiner
  protected[this] override def newCombiner: Combiner[A, CC[A]] = companion.newCombiner[A]

  def companion: GenericCompanion[CC] with GenericParCompanion[CC]
  def genericCombiner[B]: Combiner[B, CC[B]] = companion.newCombiner[B]
  override def genericBuilder[B]: Combiner[B, CC[B]] = genericCombiner[B]
}


trait GenericParMapTemplate[K, +V, +CC[X, Y] <: ParMap[X, Y]] extends GenericParTemplate[(K, V), ParIterable] {
  protected[this] override def newCombiner: Combiner[(K, V), CC[K, V]] = mapCompanion.newCombiner[K, V]
  def mapCompanion: GenericParMapCompanion[CC]
  def genericMapCombiner[P, Q]: Combiner[(P, Q), CC[P, Q]] = mapCompanion.newCombiner[P, Q]
}

