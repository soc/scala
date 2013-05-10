import annotation._
import conditional._

package object bop {
  @conditional[Debug] def debugCode[T](body: => T): T = body
  def nonDebugCode[T](body: => T): T = body
}

package bop {
  class Bippy {
    debugCode { println("hi mom") }
    nonDebugCode { println("bye mom") }
  }
}
