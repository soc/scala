/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package runtime

import java.lang.Integer.{bitCount              => javaBitCount,
                          highestOneBit         => javaHighestOneBit,
                          lowestOneBit          => javaLowestOneBit,
                          numberOfLeadingZeros  => javaNumberOfLeadingZeros,
                          numberOfTrailingZeros => javaNumberOfTrailingZeros,
                          reverse               => javaReverse,
                          reverseBytes          => javaReverseBytes,
                          rotateLeft            => javaRotateLeft,
                          rotateRight           => javaRotateRight}

import scala.collection.immutable.Range

// Note that this does not implement IntegralProxy[Int] so that it can return
// the Int-specific Range class from until/to.
final class RichInt(val self: Int) extends AnyVal with ScalaNumberProxy[Int] with RangedProxy[Int] {
  protected def num = scala.math.Numeric.IntIsIntegral
  protected def ord = scala.math.Ordering.Int
  type ResultWithoutStep = Range

  /**
    * @return `'''true'''` if this number has no decimal component.
    *         Always returns `'''true'''` for `RichInt`.
    */
  def isWhole() = true

  /**
    * @param end The final bound of the range to make.
    * @return A [[scala.collection.immutable.Range]] from `this` up to but
    *         not including `end`.
    */
  def until(end: Int): Range = Range(self, end)

  /**
    * @param end The final bound of the range to make.
    * @param step The number to increase by for each step of the range.
    * @return A [[scala.collection.immutable.Range]] from `this` up to but
    *         not including `end`.
    */
  def until(end: Int, step: Int): Range = Range(self, end, step)

  /** like `until`, but includes the last index */
  /**
    * @param end The final bound of the range to make.
    * @return A [[scala.collection.immutable.Range]] from `'''this'''` up to
    *         and including `end`.
    */
  def to(end: Int): Range.Inclusive = Range.inclusive(self, end)

  /**
    * @param end The final bound of the range to make.
    * @param step The number to increase by for each step of the range.
    * @return A [[scala.collection.immutable.Range]] from `'''this'''` up to
    *         and including `end`.
    */
  def to(end: Int, step: Int): Range.Inclusive = Range.inclusive(self, end, step)

  /**
    * @return `'''this'''` if `'''this''' < that` or `that` otherwise
    */
  override def min(that: Int): Int = if (self < that) self else that

  /**
    * @return `'''this'''` if `'''this''' > that` or `that` otherwise
    */
  override def max(that: Int): Int = if (self > that) self else that

  /**
    * Computes the absolute value of `'''this'''`.
    */
  override def abs: Int = if (self < 0) -self else self

  @inline def bitCount: Int               = javaBitCount(self)
  @inline def highestSetBit: Int          = javaHighestOneBit(self)
  @inline def lowestSetBit: Int           = javaLowestOneBit(self)
  @inline def numberOfLeadingZeros: Int   = javaNumberOfLeadingZeros(self)
  @inline def numberOfTrailingZeros: Int  = javaNumberOfTrailingZeros(self)
  @inline def reverse: Int                = javaReverse(self)
  @inline def reverseBytes: Int           = javaReverseBytes(self)
  @inline def rotateLeft(dist: Int): Int  = javaRotateLeft(self, dist)
  @inline def rotateRight(dist: Int): Int = javaRotateRight(self, dist)

  def toBinaryString: String = java.lang.Integer.toBinaryString(self)
  def toHexString: String = java.lang.Integer.toHexString(self)
  def toOctalString: String = java.lang.Integer.toOctalString(self)
}
