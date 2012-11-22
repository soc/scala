package scala.tools.partest

import scala.reflect.{ classTag, ClassTag }

trait TestUtil {
  /** Given function and block of code, evaluates code block,
   *  calls function with nanoseconds elapsed, and returns block result.
   */
  def timed[T](f: Long => Unit)(body: => T): T = {
    val start = System.nanoTime
    val result = body
    val end = System.nanoTime

    f(end - start)
    result
  }
  /** Times body and returns (nanos, result).
   */
  def alsoNanos[T](body: => T): (Long, T) = {
    var nanos = 0L
    val result = timed(nanos = _)(body)

    (nanos, result)
  }
  def nanos(body: => Unit): Long = alsoNanos(body)._1
}

// Used in tests.
object TestUtil extends TestUtil {
}
