/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

import scala.collection.{ Iterator, TraversableOnce }
import java.lang.reflect.{ Method => JMethod }
import scala.util.control.ControlThrowable

trait ScalaRuntimeTry[+A] extends Runnable {
  def Catch[B >: A](handler: PartialFunction[Throwable, B]): B
  def Finally(fin: => Unit): A
}

private[runtime] class ScalaRuntimeTryImpl[+A](block: => A) extends ScalaRuntimeTry[A] {
  private[this] var result: A = _
  private[this] var exception: Throwable =
    try   { run() ; null }
    catch {
      case e: ControlThrowable  => throw e  // don't catch non-local returns etc
      case e: Throwable         => e
    }

  def run() { result = block }

  def Catch[B >: A](handler: PartialFunction[Throwable, B]): B =
    if (exception == null) result
    else if (handler isDefinedAt exception) handler(exception)
    else throw exception

  def Finally(fin: => Unit): A = {
    fin

    if (exception == null) result
    else throw exception
  }
}
