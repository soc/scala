/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2010-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package generic

import mutable.Builder
import annotation.unchecked.uncheckedVariance
import language.higherKinds

/** This trait represents collections classes which require array
 *  tags for their element types.
 *
 *  @author Aleksandar Prokopec
 *  @since 2.8
 */
trait GenericArrayTagTraversableTemplate[+A, +CC[X] <: Traversable[X]] extends HasNewBuilder[A, CC[A] @uncheckedVariance] {
  implicit protected[this] val tag: ArrayTag[A]
  def arrayTagCompanion: GenericArrayTagCompanion[CC]
  def genericArrayTagBuilder[B](implicit tag: ArrayTag[B]): Builder[B, CC[B]] = arrayTagCompanion.newBuilder[B]
}
