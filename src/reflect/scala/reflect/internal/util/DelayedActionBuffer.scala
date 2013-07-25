/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.reflect.internal.util

import scala.annotation.tailrec
import java.util.concurrent.LinkedBlockingQueue

/** A buffer for collecting code which can't be run yet.
 */
class DelayedActionBuffer(onErr: Throwable => Unit) {
  def this() = this(Console.err println _)

  private[this] val queue = new LinkedBlockingQueue[() => Unit]

  def  run(body: => Any): Unit = queue.put(() => body)
  def show(body: => Any): Unit = run(Console.err println body)

  def showIf[T](value: T)(pf: PartialFunction[T, Any]): T =
    try value finally if (pf isDefinedAt value) show(pf(value))

  @tailrec private def drain(): Unit = queue.poll() match {
    case null =>
    case f    => try f() catch { case t: Exception => onErr(t) } ; drain()
  }
  def purge() = {
    if (!queue.isEmpty)
      Console.err.println(s"\nShutdown signal received, now performing ${queue.size} delayed actions.\n")

    drain()
  }
}
