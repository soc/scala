/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2010-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** An extension of Dynamic which abstractly defines the methods
 *  eligible to receive dynamic invocations.  These are general
 *  signatures, using `Any` in all positions; more specific
 *  signatures can be written, but because method parameter types cannot
 *  be refined in subclasses, you will want to extend Dynamic directly
 *  to do that.
 */
trait DynamicInterface extends Dynamic {
  def selectDynamic(method: String): Any
  def applyDynamic(method: String)(args: Any*): Any
  def applyDynamicNamed(method: String)(args: (String, Any)*): Any
  def updateDynamic(method: String)(args: Any*): Any
}
