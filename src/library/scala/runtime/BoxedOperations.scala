package scala.runtime

import java.{ lang => jl }

abstract class BoxesProxy extends BoxAndUnbox with BoxedEquality with BoxedOperations {
  def impl: Boxes

  def boxToBoolean(b: Boolean): jl.Boolean  = impl.boxToBoolean(b)
  def boxToCharacter(c: Char): jl.Character = impl.boxToCharacter(c)
  def boxToByte(b: Byte): jl.Byte           = impl.boxToByte(b)
  def boxToShort(s: Short): jl.Short        = impl.boxToShort(s)
  def boxToInteger(i: Int): jl.Integer      = impl.boxToInteger(i)
  def boxToLong(l: Long): jl.Long           = impl.boxToLong(l)
  def boxToFloat(f: Float): jl.Float        = impl.boxToFloat(f)
  def boxToDouble(d: Double): jl.Double     = impl.boxToDouble(d)
  def unboxToBoolean(b: AnyRef): Boolean    = impl.unboxToBoolean(b)
  def unboxToChar(c: AnyRef): Char          = impl.unboxToChar(c)
  def unboxToByte(b: AnyRef): Byte          = impl.unboxToByte(b)
  def unboxToShort(s: AnyRef): Short        = impl.unboxToShort(s)
  def unboxToInt(i: AnyRef): Int            = impl.unboxToInt(i)
  def unboxToLong(l: AnyRef): Long          = impl.unboxToLong(l)
  def unboxToFloat(f: AnyRef): Float        = impl.unboxToFloat(f)
  def unboxToDouble(d: AnyRef): Double      = impl.unboxToDouble(d)

  def equals(x: AnyRef, y: AnyRef): Boolean                  = impl.equals(x, y: AnyRef)
  def equalsNumNum(xn: jl.Number, yn: jl.Number): Boolean    = impl.equalsNumNum(xn, yn: jl.Number)
  def equalsNumObject(xn: jl.Number, y: AnyRef): Boolean     = impl.equalsNumObject(xn, y)
  def equalsCharObject(xc: jl.Character, y: AnyRef): Boolean = impl.equalsCharObject(xc, y)
  def hashFromLong(n: jl.Long): Int                          = impl.hashFromLong(n)
  def hashFromDouble(n: jl.Double): Int                      = impl.hashFromDouble(n)
  def hashFromFloat(n: jl.Float): Int                        = impl.hashFromFloat(n)
  def hashFromNumber(n: jl.Number): Int                      = impl.hashFromNumber(n)

  def add(arg1: AnyRef, arg2: AnyRef): AnyRef                    = impl.add(arg1, arg2)
  def subtract(arg1: AnyRef, arg2: AnyRef): AnyRef               = impl.subtract(arg1, arg2)
  def multiply(arg1: AnyRef, arg2: AnyRef): AnyRef               = impl.multiply(arg1, arg2)
  def divide(arg1: AnyRef, arg2: AnyRef): AnyRef                 = impl.divide(arg1, arg2)
  def takeModulo(arg1: AnyRef, arg2: AnyRef): AnyRef             = impl.takeModulo(arg1, arg2)
  def shiftSignedRight(arg1: AnyRef, arg2: AnyRef): AnyRef       = impl.shiftSignedRight(arg1, arg2)
  def shiftSignedLeft(arg1: AnyRef, arg2: AnyRef): AnyRef        = impl.shiftSignedLeft(arg1, arg2)
  def shiftLogicalRight(arg1: AnyRef, arg2: AnyRef): AnyRef      = impl.shiftLogicalRight(arg1, arg2)
  def negate(arg: AnyRef): AnyRef                                = impl.negate(arg)
  def positive(arg: AnyRef): AnyRef                              = impl.positive(arg)
  def takeAnd(arg1: AnyRef, arg2: AnyRef): AnyRef                = impl.takeAnd(arg1, arg2)
  def takeOr(arg1: AnyRef, arg2: AnyRef): AnyRef                 = impl.takeOr(arg1, arg2)
  def takeXor(arg1: AnyRef, arg2: AnyRef): AnyRef                = impl.takeXor(arg1, arg2)
  def takeConditionalAnd(arg1: AnyRef, arg2: AnyRef): AnyRef     = impl.takeConditionalAnd(arg1, arg2)
  def takeConditionalOr(arg1: AnyRef, arg2: AnyRef): AnyRef      = impl.takeConditionalOr(arg1, arg2)
  def complement(arg: AnyRef): AnyRef                            = impl.complement(arg)
  def takeNot(arg: AnyRef): AnyRef                               = impl.takeNot(arg)
  def testEqual(arg1: AnyRef, arg2: AnyRef): AnyRef              = impl.testEqual(arg1, arg2)
  def testNotEqual(arg1: AnyRef, arg2: AnyRef): AnyRef           = impl.testNotEqual(arg1, arg2)
  def testLessThan(arg1: AnyRef, arg2: AnyRef): AnyRef           = impl.testLessThan(arg1, arg2)
  def testLessOrEqualThan(arg1: AnyRef, arg2: AnyRef): AnyRef    = impl.testLessOrEqualThan(arg1, arg2)
  def testGreaterOrEqualThan(arg1: AnyRef, arg2: AnyRef): AnyRef = impl.testGreaterOrEqualThan(arg1, arg2)
  def testGreaterThan(arg1: AnyRef, arg2: AnyRef): AnyRef        = impl.testGreaterThan(arg1, arg2)
  def isBoxedNumberOrBoolean(arg: AnyRef): Boolean               = impl.isBoxedNumberOrBoolean(arg)
  def isBoxedNumber(arg: AnyRef): Boolean                        = impl.isBoxedNumber(arg)
  def toCharacter(arg: AnyRef): jl.Character                     = impl.toCharacter(arg)
  def toByte(arg: AnyRef): jl.Byte                               = impl.toByte(arg)
  def toShort(arg: AnyRef): jl.Short                             = impl.toShort(arg)
  def toInteger(arg: AnyRef): jl.Integer                         = impl.toInteger(arg)
  def toLong(arg: AnyRef): jl.Long                               = impl.toLong(arg)
  def toFloat(arg: AnyRef): jl.Float                             = impl.toFloat(arg)
  def toDouble(arg: AnyRef): jl.Double                           = impl.toDouble(arg)
}

trait BoxAndUnbox {
  def boxToBoolean(b: Boolean): jl.Boolean
  def boxToCharacter(c: Char): jl.Character
  def boxToByte(b: Byte): jl.Byte
  def boxToShort(s: Short): jl.Short
  def boxToInteger(i: Int): jl.Integer
  def boxToLong(l: Long): jl.Long
  def boxToFloat(f: Float): jl.Float
  def boxToDouble(d: Double): jl.Double
  def unboxToBoolean(b: AnyRef): Boolean
  def unboxToChar(c: AnyRef): Char
  def unboxToByte(b: AnyRef): Byte
  def unboxToShort(s: AnyRef): Short
  def unboxToInt(i: AnyRef): Int
  def unboxToLong(l: AnyRef): Long
  def unboxToFloat(f: AnyRef): Float
  def unboxToDouble(d: AnyRef): Double
}

trait BoxedEquality {
  def equals(x: AnyRef, y: AnyRef): Boolean
  def equalsNumNum(xn: jl.Number, yn: jl.Number): Boolean
  def equalsNumObject(xn: jl.Number, y: AnyRef): Boolean
  def equalsCharObject(xc: jl.Character, y: AnyRef): Boolean
  def hashFromLong(n: jl.Long): Int
  def hashFromDouble(n: jl.Double): Int
  def hashFromFloat(n: jl.Float): Int
  def hashFromNumber(n: jl.Number): Int
  def isBoxedNumberOrBoolean(arg: AnyRef): Boolean
  def isBoxedNumber(arg: AnyRef): Boolean
}

trait BoxedOperations {
  @throws(classOf[NoSuchMethodException]) def add(arg1: AnyRef, arg2: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def subtract(arg1: AnyRef, arg2: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def multiply(arg1: AnyRef, arg2: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def divide(arg1: AnyRef, arg2: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def takeModulo(arg1: AnyRef, arg2: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def shiftSignedRight(arg1: AnyRef, arg2: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def shiftSignedLeft(arg1: AnyRef, arg2: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def shiftLogicalRight(arg1: AnyRef, arg2: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def negate(arg: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def positive(arg: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def takeAnd(arg1: AnyRef, arg2: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def takeOr(arg1: AnyRef, arg2: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def takeXor(arg1: AnyRef, arg2: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def takeConditionalAnd(arg1: AnyRef, arg2: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def takeConditionalOr(arg1: AnyRef, arg2: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def complement(arg: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def takeNot(arg: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def testEqual(arg1: AnyRef, arg2: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def testNotEqual(arg1: AnyRef, arg2: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def testLessThan(arg1: AnyRef, arg2: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def testLessOrEqualThan(arg1: AnyRef, arg2: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def testGreaterOrEqualThan(arg1: AnyRef, arg2: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def testGreaterThan(arg1: AnyRef, arg2: AnyRef): AnyRef
  @throws(classOf[NoSuchMethodException]) def toCharacter(arg: AnyRef): jl.Character
  @throws(classOf[NoSuchMethodException]) def toByte(arg: AnyRef): jl.Byte
  @throws(classOf[NoSuchMethodException]) def toShort(arg: AnyRef): jl.Short
  @throws(classOf[NoSuchMethodException]) def toInteger(arg: AnyRef): jl.Integer
  @throws(classOf[NoSuchMethodException]) def toLong(arg: AnyRef): jl.Long
  @throws(classOf[NoSuchMethodException]) def toFloat(arg: AnyRef): jl.Float
  @throws(classOf[NoSuchMethodException]) def toDouble(arg: AnyRef): jl.Double
}
