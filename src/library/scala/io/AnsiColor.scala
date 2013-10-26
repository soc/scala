package scala
package io

import AnsiCode._

trait AnsiColor {
  /** Foreground color for ANSI black */
  final val BLACK      = "\u001b[30m"
  /** Foreground color for ANSI red */
  final val RED        = "\u001b[31m"
  /** Foreground color for ANSI green */
  final val GREEN      = "\u001b[32m"
  /** Foreground color for ANSI yellow */
  final val YELLOW     = "\u001b[33m"
  /** Foreground color for ANSI blue */
  final val BLUE       = "\u001b[34m"
  /** Foreground color for ANSI magenta */
  final val MAGENTA    = "\u001b[35m"
  /** Foreground color for ANSI cyan */
  final val CYAN       = "\u001b[36m"
  /** Foreground color for ANSI white */
  final val WHITE      = "\u001b[37m"

  /** Background color for ANSI black */
  final val BLACK_B    = "\u001b[40m"
  /** Background color for ANSI red */
  final val RED_B      = "\u001b[41m"
  /** Background color for ANSI green */
  final val GREEN_B    = "\u001b[42m"
  /** Background color for ANSI yellow */
  final val YELLOW_B   = "\u001b[43m"
  /** Background color for ANSI blue */
  final val BLUE_B     = "\u001b[44m"
  /** Background color for ANSI magenta */
  final val MAGENTA_B  = "\u001b[45m"
  /** Background color for ANSI cyan */
  final val CYAN_B     = "\u001b[46m"
  /** Background color for ANSI white */
  final val WHITE_B    = "\u001b[47m"

  /** Reset ANSI styles */
  final val RESET      = "\u001b[0m"
  /** ANSI bold */
  final val BOLD       = "\u001b[1m"
  /** ANSI underlines */
  final val UNDERLINED = "\u001b[4m"
  /** ANSI blink */
  final val BLINK      = "\u001b[5m"
  /** ANSI reversed */
  final val REVERSED   = "\u001b[7m"
  /** ANSI invisible */
  final val INVISIBLE  = "\u001b[8m"
}

import AnsiCode._

final class AnsiCode private (val value: Int) extends AnyVal {
  def isForeground = 30 <= value && value < 40
  def isBackground = 40 <= value && value < 50
  def flip: AnsiCode    = new AnsiCode(flipValue)

  def code = "\u001b[" + value + "m"
  def colorize(s: String): String = code + s + Reset

  private def flipValue = if (isForeground) value + 10 else if (isBackground) value - 10 else value
  override def toString = code
}

object AnsiCode {
  private def code(n: Int): AnsiCode = new AnsiCode(n)

  val Reset      = code(0)
  val Bold       = code(1)
  val Underlined = code(4)
  val Blink      = code(5)
  val Reversed   = code(7)
  val Invisible  = code(8)
  val Black      = code(30)
  val Red        = code(31)
  val Green      = code(32)
  val Yellow     = code(33)
  val Blue       = code(34)
  val Magenta    = code(35)
  val Cyan       = code(36)
  val White      = code(37)
}

object AnsiColor extends AnsiColor {
}
