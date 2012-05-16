/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package generic

import mutable.Builder
import language.higherKinds

abstract class SetFactory[CC[X] <: Set[X] with SetLike[X, CC[X]]] extends GenericSeqCompanion[CC] {
  def newBuilder[A]: Builder[A, CC[A]]

  /** $setCanBuildFromInfo
   */
  def setCanBuildFrom[A] = new CanBuildFrom[CC[_], A, CC[A]] {
    def apply(from: CC[_]) = newBuilder[A]
    def apply() = newBuilder[A]
  }
}