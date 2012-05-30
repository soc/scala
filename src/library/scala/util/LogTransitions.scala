package scala.util

abstract class LogTransitions[S](onEnter: S => String, onExit: S => String) {
  def enabled = false
  def log(msg: String): Unit

  @inline final def apply[T](entity: S)(body: => T): T = {
    if (enabled) log(onEnter(entity))
    try body
    finally if (enabled) log(onExit(entity))
  }
}
