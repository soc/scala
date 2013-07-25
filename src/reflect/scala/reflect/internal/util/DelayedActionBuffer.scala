/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.reflect.internal.util

import java.util.concurrent.LinkedBlockingQueue

class DelayedActionBuffer(onErr: Throwable => Unit) {
  private val errHandler: PartialFunction[Throwable, Unit] = { case t => onErr(t) }
  @volatile private[this] var closed = false
  private[this] val queue = new LinkedBlockingQueue[() => Unit]

  def close(): Unit            = closed = true
  def  run(body: => Any): Unit = if (!closed) queue.put(() => body)
  def show(body: => Any): Unit = run(Console.err println body)
  def maybe(value: T)(pf: PartialFunction[T, Any]): T = {
    if (pf isDefinedAt value)
      run(pf(value))

    value
  }
  @tailrec final def purge() = queue.poll() match {
    case null =>
    case f    => try f() catch errHandler ; purge()
  }
  /** Queue an entry to run, print, or conditionally print the
   *  given body.
   */
  def add(action: => Unit) = maybe(actions += (() => action))
  def show(msg: => Any)    = maybe(add(Console.err println msg))
  def showIf[T](value: T)(pf: PartialFunction[T, Any]): T =
    try value finally maybe(if (pf isDefinedAt value) show(pf(value)))

  // Set a shutdown hook to purge the queue.
  // Close it first in case the actions will try to enqueue more.
  def purgeAtShutdown(): Unit = scala.sys addShutdownHook { close() ; purge() }
}
