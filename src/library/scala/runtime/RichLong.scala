/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package runtime

import java.lang.Long.{bitCount              => javaBitCount,
                       highestOneBit         => javaHighestOneBit,
                       lowestOneBit          => javaLowestOneBit,
                       numberOfLeadingZeros  => javaNumberOfLeadingZeros,
                       numberOfTrailingZeros => javaNumberOfTrailingZeros,
                       reverse               => javaReverse,
                       reverseBytes          => javaReverseBytes,
                       rotateLeft            => javaRotateLeft,
                       rotateRight           => javaRotateRight}

final class RichLong(val self: Long) extends AnyVal with IntegralProxy[Long] {
  protected def num = scala.math.Numeric.LongIsIntegral
  protected def ord = scala.math.Ordering.Long

  def toBinaryString: String = java.lang.Long.toBinaryString(self)
  def toHexString: String = java.lang.Long.toHexString(self)
  def toOctalString: String = java.lang.Long.toOctalString(self)

  override def isValidByte = self.toByte.toLong == self
  override def isValidShort = self.toShort.toLong == self
  override def isValidChar = self.toChar.toLong == self
  override def isValidInt = self.toInt.toLong == self
  // override def isValidLong = true
  // override def isValidFloat = self.toFloat.toLong == self && self != Long.MaxValue
  // override def isValidDouble = self.toDouble.toLong == self && self != Long.MaxValue

  @inline def bitCount: Int                = javaBitCount(self)
  @inline def highestSetBit: Long          = javaHighestOneBit(self)
  @inline def lowestSetBit: Long           = javaLowestOneBit(self)
  @inline def numberOfLeadingZeros: Int    = javaNumberOfLeadingZeros(self)
  @inline def numberOfTrailingZeros: Int   = javaNumberOfTrailingZeros(self)
  @inline def reverse: Long                = javaReverse(self)
  @inline def reverseBytes: Long           = javaReverseBytes(self)
  @inline def rotateLeft(dist: Int): Long  = javaRotateLeft(self, dist)
  @inline def rotateRight(dist: Int): Long = javaRotateRight(self, dist)
}
