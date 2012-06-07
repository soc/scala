/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.math

import java.{ lang => jl, math => jm }
import language.implicitConversions

/**
 *  @author  Stephane Micheloud
 *  @version 1.0
 *  @since 2.7
 */
object Decimal {
  private val minCached = -512
  private val maxCached = 512
  val defaultMathContext = jm.MathContext.DECIMAL128

  /** Cache ony for defaultMathContext using Decimals in a small range. */
  private lazy val cache = new Array[Decimal](maxCached - minCached + 1)

  /** Constructs a `Decimal` using the java Decimal static
   *  valueOf constructor.
   *
   *  @param  d the specified double value
   *  @return the constructed `Decimal`
   */
  def valueOf(d: Double): Decimal = apply(jm.BigDecimal valueOf d)
  def valueOf(d: Double, mc: jm.MathContext): Decimal = apply(jm.BigDecimal valueOf d, mc)

  /** Constructs a `Decimal` whose value is equal to that of the
   *  specified `Integer` value.
   *
   *  @param i the specified integer value
   *  @return  the constructed `Decimal`
   */
  def apply(i: Int): Decimal = apply(i, defaultMathContext)
  def apply(i: Int, mc: jm.MathContext): Decimal =
    if (mc == defaultMathContext && minCached <= i && i <= maxCached) {
      val offset = i - minCached
      var n = cache(offset)
      if (n eq null) { n = new Decimal(jm.BigDecimal.valueOf(i), mc); cache(offset) = n }
      n
    }
    else new Decimal(jm.BigDecimal.valueOf(i), mc)

  /** Constructs a `Decimal` whose value is equal to that of the
   *  specified long value.
   *
   *  @param l the specified long value
   *  @return  the constructed `Decimal`
   */
  def apply(l: Long): Decimal =
    if (minCached <= l && l <= maxCached) apply(l.toInt)
    else new Decimal(jm.BigDecimal.valueOf(l), defaultMathContext)

  def apply(l: Long, mc: jm.MathContext): Decimal =
    new Decimal(new jm.BigDecimal(l, mc), mc)

  /** Constructs a `Decimal` whose unscaled value is equal to that
   *  of the specified long value.
   *
   *  @param  unscaledVal the value
   *  @param  scale       the scale
   *  @return the constructed `Decimal`
   */
  def apply(unscaledVal: Long, scale: Int): Decimal =
    apply(BigInt(unscaledVal), scale)

  def apply(unscaledVal: Long, scale: Int, mc: jm.MathContext): Decimal =
    apply(BigInt(unscaledVal), scale, mc)

  /** Constructs a `Decimal` whose value is equal to that of the
   *  specified double value.
   *
   *  @param d the specified `Double` value
   *  @return  the constructed `Decimal`
   */
  def apply(d: Double): Decimal = apply(d, defaultMathContext)
  // note we don't use the static valueOf because it doesn't let us supply
  // a jm.MathContext, but we should be duplicating its logic, modulo caching.
  def apply(d: Double, mc: jm.MathContext): Decimal =
    new Decimal(new jm.BigDecimal(jl.Double.toString(d), mc), mc)

  /** Translates a character array representation of a `Decimal`
   *  into a `Decimal`.
   */
  def apply(x: Array[Char]): Decimal = apply(x, defaultMathContext)
  def apply(x: Array[Char], mc: jm.MathContext): Decimal =
    new Decimal(new jm.BigDecimal(x.mkString, mc), mc)

  /** Translates the decimal String representation of a `Decimal`
   *  into a `Decimal`.
   */
  def apply(x: String): Decimal = apply(x, defaultMathContext)
  def apply(x: String, mc: jm.MathContext): Decimal =
    new Decimal(new jm.BigDecimal(x, mc), mc)

  /** Constructs a `Decimal` whose value is equal to that of the
   *  specified `BigInt` value.
   *
   *  @param x the specified `BigInt` value
   *  @return  the constructed `Decimal`
   */
  def apply(x: BigInt): Decimal = apply(x, defaultMathContext)
  def apply(x: BigInt, mc: jm.MathContext): Decimal =
    new Decimal(new jm.BigDecimal(x.bigInteger, mc), mc)

  /** Constructs a `Decimal` whose unscaled value is equal to that
   *  of the specified `BigInt` value.
   *
   *  @param unscaledVal the specified `BigInt` value
   *  @param scale       the scale
   *  @return  the constructed `Decimal`
   */
  def apply(unscaledVal: BigInt, scale: Int): Decimal = apply(unscaledVal, scale, defaultMathContext)
  def apply(unscaledVal: BigInt, scale: Int, mc: jm.MathContext): Decimal =
    new Decimal(new jm.BigDecimal(unscaledVal.bigInteger, scale, mc), mc)

  def apply(bd: jm.BigDecimal): Decimal = apply(bd, defaultMathContext)
  def apply(bd: jm.BigDecimal, mc: jm.MathContext): Decimal = new Decimal(bd, mc)

  /** Implicit conversion from `Int` to `Decimal`. */
  implicit def int2bigDecimal(i: Int): Decimal = apply(i)

  /** Implicit conversion from `Long` to `Decimal`. */
  implicit def long2bigDecimal(l: Long): Decimal = apply(l)

  /** Implicit conversion from `Double` to `Decimal`. */
  implicit def double2bigDecimal(d: Double): Decimal = valueOf(d, defaultMathContext)

  /** Implicit conversion from `java.math.BigDecimal` to `scala.Decimal`. */
  implicit def javaDecimal2bigDecimal(x: jm.BigDecimal): Decimal = apply(x)
}

/**
 *  @author  Stephane Micheloud
 *  @version 1.0
 */
class Decimal(
  val bigDecimal: jm.BigDecimal,
  val mc: jm.MathContext)
extends ScalaNumber with ScalaNumericConversions with Serializable {
  def this(bigDecimal: jm.BigDecimal) = this(bigDecimal, Decimal.defaultMathContext)

  /** Cuts way down on the wrapper noise. */
  private implicit def bigdec2Decimal(x: jm.BigDecimal): Decimal = new Decimal(x, mc)

  /** Returns the hash code for this Decimal.
   *  Note that this does not use the underlying java object's
   *  hashCode because we compare Decimals with compareTo
   *  which deems 2 == 2.00, whereas in java these are unequal
   *  with unequal hashCodes.
   */
  override def hashCode(): Int = bigDecimal.doubleValue.##

  /** Compares this Decimal with the specified value for equality.
   */
  override def equals (that: Any): Boolean = that match {
    case that: Decimal => this equals that
    case that: BigInt     => this.toBigIntExact exists (that equals _)
    case _                => super.equals(that)
  }
  protected[math] def isWhole = (this remainder 1) == Decimal(0)
  def underlying = bigDecimal

  /** Compares this Decimal with the specified Decimal for equality.
   */
  def equals (that: Decimal): Boolean = compare(that) == 0

  /** Compares this Decimal with the specified Decimal
   */
  def compare (that: Decimal): Int = this.bigDecimal compareTo that.bigDecimal

  /** Less-than-or-equals comparison of Decimals
   */
  def <= (that: Decimal): Boolean = compare(that) <= 0

  /** Greater-than-or-equals comparison of Decimals
   */
  def >= (that: Decimal): Boolean = compare(that) >= 0

  /** Less-than of Decimals
   */
  def <  (that: Decimal): Boolean = compare(that) <  0

  /** Greater-than comparison of Decimals
   */
  def >  (that: Decimal): Boolean = compare(that) > 0

  /** Addition of Decimals
   */
  def +  (that: Decimal): Decimal = this.bigDecimal.add(that.bigDecimal)

  /** Subtraction of Decimals
   */
  def -  (that: Decimal): Decimal = this.bigDecimal.subtract(that.bigDecimal)

  /** Multiplication of Decimals
   */
  def *  (that: Decimal): Decimal = this.bigDecimal.multiply(that.bigDecimal, mc)

  /** Division of Decimals
   */
  def /  (that: Decimal): Decimal = this.bigDecimal.divide(that.bigDecimal, mc)

  /** Division and Remainder - returns tuple containing the result of
   *  divideToIntegralValue and the remainder.
   */
  def /% (that: Decimal): (Decimal, Decimal) =
    this.bigDecimal.divideAndRemainder(that.bigDecimal) match {
      case Array(q, r)  => (q, r)
    }

  /** Divide to Integral value.
   */
  def quot (that: Decimal): Decimal =
    this.bigDecimal.divideToIntegralValue(that.bigDecimal)

  /** Returns the minimum of this and that
   */
  def min (that: Decimal): Decimal = this.bigDecimal min that.bigDecimal

  /** Returns the maximum of this and that
   */
  def max (that: Decimal): Decimal = this.bigDecimal max that.bigDecimal

  /** Remainder after dividing this by that.
   */
  def remainder (that: Decimal): Decimal = this.bigDecimal.remainder(that.bigDecimal)

  /** Remainder after dividing this by that.
   */
  def % (that: Decimal): Decimal = this.remainder(that)

  /** Returns a Decimal whose value is this ** n.
   */
  def pow (n: Int): Decimal = this.bigDecimal.pow(n, mc)

  /** Returns a Decimal whose value is the negation of this Decimal
   */
  def unary_- : Decimal = this.bigDecimal.negate()

  /** Returns the absolute value of this Decimal
   */
  def abs: Decimal = this.bigDecimal.abs

  /** Returns the sign of this Decimal, i.e.
   *   -1 if it is less than 0,
   *   +1 if it is greater than 0
   *   0  if it is equal to 0
   */
  def signum: Int = this.bigDecimal.signum()

  /** Returns the precision of this `Decimal`.
   */
  def precision: Int = this.bigDecimal.precision()

  /** Returns a Decimal rounded according to the jm.MathContext settings.
   */
  def round(mc: jm.MathContext): Decimal = this.bigDecimal round mc

  /** Returns the scale of this `Decimal`.
   */
  def scale: Int = this.bigDecimal.scale()

  /** Returns the size of an ulp, a unit in the last place, of this Decimal.
   */
  def ulp: Decimal = this.bigDecimal.ulp

  /** Returns a new Decimal based on the supplied jm.MathContext.
   */
  def apply(mc: jm.MathContext): Decimal = Decimal(this.bigDecimal.toString, mc)

  /** Returns a `Decimal` whose scale is the specified value, and whose value is
   *  numerically equal to this Decimal's.
   */
  def setScale(scale: Int): Decimal = this.bigDecimal setScale scale

  def setScale(scale: Int, mode: jm.RoundingMode): Decimal =
    this.bigDecimal.setScale(scale, mode)

  /** Converts this Decimal to a Byte.
   *  If the Decimal is too big to fit in a Byte, only the low-order 8 bits are returned.
   *  Note that this conversion can lose information about the overall magnitude of the
   *  Decimal value as well as return a result with the opposite sign.
   */
  override def byteValue   = intValue.toByte

  /** Converts this Decimal to a Short.
   *  If the Decimal is too big to fit in a Byte, only the low-order 16 bits are returned.
   *  Note that this conversion can lose information about the overall magnitude of the
   *  Decimal value as well as return a result with the opposite sign.
   */
  override def shortValue  = intValue.toShort

  /** Converts this Decimal to a Char.
   *  If the Decimal is too big to fit in a char, only the low-order 16 bits are returned.
   *  Note that this conversion can lose information about the overall magnitude of the
   *  Decimal value and that it always returns a positive result.
   */
  def charValue   = intValue.toChar

  /** Converts this Decimal to an Int.
   *  If the Decimal is too big to fit in a char, only the low-order 32 bits
   *  are returned. Note that this conversion can lose information about the
   *  overall magnitude of the Decimal value as well as return a result with
   *  the opposite sign.
   */
  def intValue    = this.bigDecimal.intValue

  /** Converts this Decimal to a Long.
   *  If the Decimal is too big to fit in a char, only the low-order 64 bits
   *  are returned. Note that this conversion can lose information about the
   *  overall magnitude of the Decimal value as well as return a result with
   *  the opposite sign.
   */
  def longValue   = this.bigDecimal.longValue

  /** Converts this Decimal to a Float.
   *  if this Decimal has too great a magnitude to represent as a float,
   *  it will be converted to `Float.NEGATIVE_INFINITY` or
   *  `Float.POSITIVE_INFINITY` as appropriate.
   */
  def floatValue  = this.bigDecimal.floatValue

  /** Converts this Decimal to a Double.
   *  if this Decimal has too great a magnitude to represent as a double,
   *  it will be converted to `Double.NEGATIVE_INFINITY` or
   *  `Double.POSITIVE_INFINITY` as appropriate.
   */
  def doubleValue = this.bigDecimal.doubleValue

  /** Converts this `Decimal` to a [[scala.Byte]], checking for lost information.
    * If this `Decimal` has a nonzero fractional part, or is out of the possible
    * range for a [[scala.Byte]] result, then a `java.lang.ArithmeticException` is
    * thrown.
    */
  def toByteExact = bigDecimal.byteValueExact

  /** Converts this `Decimal` to a [[scala.Short]], checking for lost information.
    * If this `Decimal` has a nonzero fractional part, or is out of the possible
    * range for a [[scala.Short]] result, then a `java.lang.ArithmeticException` is
    * thrown.
    */
  def toShortExact = bigDecimal.shortValueExact

  /** Converts this `Decimal` to a [[scala.Int]], checking for lost information.
    * If this `Decimal` has a nonzero fractional part, or is out of the possible
    * range for an [[scala.Int]] result, then a `java.lang.ArithmeticException` is
    * thrown.
    */
  def toIntExact = bigDecimal.intValueExact

  /** Converts this `Decimal` to a [[scala.Long]], checking for lost information.
    * If this `Decimal` has a nonzero fractional part, or is out of the possible
    * range for a [[scala.Long]] result, then a `java.lang.ArithmeticException` is
    * thrown.
    */
  def toLongExact = bigDecimal.longValueExact

  /** Converts this `Decimal` to a scala.BigInt.
   */
  def toBigInt(): BigInt = new BigInt(this.bigDecimal.toBigInteger())

  /** Converts this `Decimal` to a scala.BigInt if it
   *  can be done losslessly, returning Some(BigInt) or None.
   */
  def toBigIntExact(): Option[BigInt] =
    try Some(new BigInt(this.bigDecimal.toBigIntegerExact()))
    catch { case _: ArithmeticException => None }

  /** Returns the decimal String representation of this Decimal.
   */
  override def toString(): String = this.bigDecimal.toString()

}
