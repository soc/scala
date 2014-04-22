/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package reflect

/** A `OptManifest[T]` is an optional [[scala.reflect.Manifest]].
 *
 *  It is either a `Manifest` or the value `NoManifest`.
 *
 *  @author Martin Odersky
 */
@deprecated("This notion doesn't have a corresponding concept in the tag hierarchy, because scala.reflect.runtime.universe.TypeTag can capture arbitrary types. Use type tags instead of manifests, and there will be no need in opt manifests.", "2.11.0")
trait OptManifest[+T] extends Serializable