/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

import scala.concurrent._
import scala.concurrent.duration._

/** A means of exposing all the operations of some T yet still
 *  enforcing that some initialization takes place before any
 *  of them are called.
 */
abstract class Init[T] {
  val op: Future[T] = future(futureOp)(executionContext)

  protected def futureOp: T
  protected def timeout: Duration
  protected def executionContext: ExecutionContext

  def isCompleted = op.isCompleted
  def value: T = Await.result(op, timeout)
  def block(max: Duration = timeout): Unit = Await.result(op, max)
}
object Init {
  def apply[T](body: => T)(implicit ctx: ExecutionContext = ExecutionContext.Implicits.global): Init[T] = {
    new Init[T] {
      protected def futureOp         = body
      protected def timeout          = Duration.Inf
      protected def executionContext = ctx
    }
  }
  implicit def unwrapOrBlock[T](wrapper: Init[T]): T = wrapper.value
}
