/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.repl

import scala.tools.nsc._
import util.Exceptional.unwrap
import util.stackTraceString

trait ReplConfig {
  lazy val replProps = new ReplProps

  private def echo(msg: => String) =
    try Console println msg
    catch { case x: AssertionError => Console.println("Assertion error printing debugging output: " + x) }

  private[repl] def repldbgex(ex: Throwable): Unit = {
    if (isReplDebug) {
      echo("Caught/suppressing: " + ex)
      ex.printStackTrace
    }
  }
  private[repl] def replLog[T](msg: PartialFunction[T, String])(value: T): T = {
    if (isReplDebug && msg.isDefinedAt(value))
      echo(msg(value))

    value
  }
  private[repl] def repldbg(msg: => String)    = if (isReplDebug) echo(msg)
  private[repl] def repltrace(msg: => String)  = if (isReplTrace) echo(msg)
  private[repl] def replinfo(msg: => String)   = if (isReplInfo)  echo(msg)

  private[repl] def logAndDiscard[T](label: String, alt: => T): PartialFunction[Throwable, T] = {
    case t =>
      repldbg(label + ": " + unwrap(t))
      repltrace(stackTraceString(unwrap(t)))
      alt
  }
  private[repl] def substituteAndLog[T](alt: => T)(body: => T): T =
    substituteAndLog("" + alt, alt)(body)
  private[repl] def substituteAndLog[T](label: String, alt: => T)(body: => T): T = {
    try body
    catch logAndDiscard(label, alt)
  }
  private[repl] def squashAndLog(label: String)(body: => Unit): Unit =
    substituteAndLog(label, ())(body)

  def isReplTrace: Boolean = replProps.trace
  def isReplDebug: Boolean = replProps.debug || isReplTrace
  def isReplInfo: Boolean  = replProps.info || isReplDebug
}
