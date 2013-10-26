package scala
package io

import AnsiCode._

final class AnsiCode private (val value: Int) extends AnyVal {
  def isForeground = 30 <= value && value < 40
  def isBackground = 40 <= value && value < 50
  def flip: AnsiCode = new AnsiCode(flipValue)

  def code = Esc + value + "m"
  def colorize(s: String): String = code + s + Reset

  private def flipValue = if (isForeground) value + 10 else if (isBackground) value - 10 else value
  override def toString = code
}

object AnsiCode {
  private def code(n: Int): AnsiCode = new AnsiCode(n)

  final val Esc = "\u001b["

  final val BoldBit = 1 << 1
  final val UnderlineBit = 1 << 2
  final val BlinkBit = 1 << 3
  final val ReverseBit = 1 << 4

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

  object LeadingCode {
    def unapply(s: String) = (
      if (s startsWith "\033[") Some(s splitAt (s indexOf 'm') + 1)
      else None
    )
  }
  object TrailingCode {
    def unapply(s: String) = s lastIndexOf Esc match {
      case -1  => None
      case idx =>
        val (f, b) = s splitAt idx
        if (b.length <= 5) Some(f -> b) else None
    }
  }
  object CodesAndString {
    def unapply(s: String): Some[(String, String, String)] = s match {
      case LeadingCode(code, rest)  => unapply(rest) match { case Some((x, y, z)) => Some((code + x, y, z)) }
      case TrailingCode(rest, code) => unapply(rest) match { case Some((x, y, z)) => Some((x, y, z + code)) }
      case _                        => Some(("", s, ""))
    }
  }

  def inColor(s: String, color: String): String   = color +            s + RESET
  def inBold(s: String, color: String): String    = color + BOLD     + s + RESET
  def inReverse(s: String, color: String): String = color + REVERSED + s + RESET
  def resetColor(s: String): String               =                    s + RESET

  implicit class AnsiCodeStringOps(val s: String) extends AnyVal {
    private def addModifier(mod: String) = s match {
      case CodesAndString(x, y, z) => x + mod + y + z
    }
    def reset    = s + RESET
    def reversed = addModifier(REVERSED)
    def bold     = addModifier(BOLD)

    def inColor(color: AnsiCode): String = color colorize s

    def inLightRed     = inColor(Red)
    def inLightGreen   = inColor(Green)
    def inLightBlue    = inColor(Blue)
    def inLightMagenta = inColor(Magenta)
    def inLightCyan    = inColor(Cyan)

    def inRed          = inBold(s, RED)
    def inGreen        = inBold(s, GREEN)
    def inBlue         = inBold(s, BLUE)
    def inMagenta      = inBold(s, MAGENTA)
    def inCyan         = inBold(s, CYAN)
  }
}
