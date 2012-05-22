/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

import java.lang.Character

final class RichChar(val ch: Char) extends AnyVal {
  def isControl                = Character.isISOControl(ch)
  def isDefined                = Character.isDefined(ch)
  def isDigit                  = Character.isDigit(ch)
  def isHighSurrogate          = Character.isHighSurrogate(ch)
  def isIdentifierIgnorable    = Character.isIdentifierIgnorable(ch)
  def isJavaIdentifierPart     = Character.isJavaIdentifierPart(ch)
  def isJavaIdentifierStart    = Character.isJavaIdentifierStart(ch)
  def isLetter                 = Character.isLetter(ch)
  def isLetterOrDigit          = Character.isLetterOrDigit(ch)
  def isLowSurrogate           = Character.isLowSurrogate(ch)
  def isLower                  = Character.isLowerCase(ch)
  def isMirrored               = Character.isMirrored(ch)
  def isSpaceChar              = Character.isSpaceChar(ch)
  def isSurrogate              = isHighSurrogate || isLowSurrogate
  def isTitleCase              = Character.isTitleCase(ch)
  def isUnicodeIdentifierPart  = Character.isUnicodeIdentifierPart(ch)
  def isUnicodeIdentifierStart = Character.isUnicodeIdentifierStart(ch)
  def isUpper                  = Character.isUpperCase(ch)
  def isWhitespace             = Character.isWhitespace(ch)

  def asDigit: Int            = Character.digit(ch, Character.MAX_RADIX)
  def getDirectionality: Byte = Character.getDirectionality(ch)
  def getNumericValue: Int    = Character.getNumericValue(ch)
  def getType: Int            = Character.getType(ch)
  def reverseBytes: Char      = Character.reverseBytes(ch)
  def toLower: Char           = Character.toLowerCase(ch)
  def toTitleCase: Char       = Character.toTitleCase(ch)
  def toUpper: Char           = Character.toUpperCase(ch)
}
