/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

class InstrumentedScalaRunTime extends StandardScalaRunTime {
  var arrayApplyCount = 0
  var arrayUpdateCount = 0

  def printArrayCounts() {
    Console.println("Arrays apply=%d update=%d".format(arrayApplyCount, arrayUpdateCount))
  }

  override def array_apply(xs: AnyRef, idx: Int): Any = {
    arrayApplyCount += 1
    super.array_apply(xs, idx)
  }
  override def array_update(xs: AnyRef, idx: Int, value: Any) {
    arrayUpdateCount += 1
    super.array_update(xs, idx, value)
  }
}
