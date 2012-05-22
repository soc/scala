/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.math

import java.{ lang => jl }

/** Conversions which present a consistent conversion interface
 *  across all the numeric types.
 */
trait ScalaNumericConversions extends ScalaNumber {
  /** Returns the value of this as a [[scala.Char]]. This may involve
    * rounding or truncation.
    */
  def toChar = intValue.toChar

  /** Returns the value of this as a [[scala.Byte]]. This may involve
    * rounding or truncation.
    */
  def toByte = byteValue

  /** Returns the value of this as a [[scala.Short]]. This may involve
    * rounding or truncation.
    */
  def toShort = shortValue

  /** Returns the value of this as an [[scala.Int]]. This may involve
    * rounding or truncation.
    */
  def toInt = intValue

  /** Returns the value of this as a [[scala.Long]]. This may involve
    * rounding or truncation.
    */
  def toLong = longValue

  /** Returns the value of this as a [[scala.Float]]. This may involve
    * rounding or truncation.
    */
  def toFloat = floatValue

  /** Returns the value of this as a [[scala.Double]]. This may involve
    * rounding or truncation.
    */
  def toDouble = doubleValue
}
