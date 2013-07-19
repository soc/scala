package scala
package reflect
package io
package classpath

import Debug._

/** Public members for reuse elsewhere.
 */
object Debug {
  final val isDebug    = sys.props contains "cp.debug"
  final val isParallel = sys.props contains "cp.parallel"
  final val isCache    = sys.props contains "cp.cache"

  def logErr(msg: => String): Unit = if (isDebug) printErr(msg)
  def printErr(msg: String): Unit  = Console.err println msg
  def pokeErr(msg: String): Unit   = Console.err print msg

  @inline final def timed[T](msg: T => String)(body: => T): T = {
    val start   = System.nanoTime
    val result  = body
    val elapsed = System.nanoTime - start
    val ms      = "%.3f" format elapsed / 1e6

    printErr(s"[%8s ms] %s".format(ms, msg(result)))
    result
  }
}

/** Package private so as to be mixed into the package object and
 *  available in the package without being importable elsewhere.
 */
trait PackageDebug {
  private[classpath] def timed[T](msg: T => String)(body: => T): T =
    if (isDebug) Debug.timed(msg)(body) else body

  private[classpath] def trace[T](msg: String)(res: T): T = {
    logErr(msg + ": " + res)
    res
  }
}
