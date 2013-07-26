/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

import java.{ lang => jl }

class InstrumentedScalaRunTime extends StandardScalaRunTime {
  var arrayApplyCount = 0
  var arrayUpdateCount = 0

  def printArrayCounts() {
    Console.println("Arrays apply=%d update=%d".format(arrayApplyCount, arrayUpdateCount))
  }

  override def array_apply(xs: AnyRef, idx: Int): Any = {
    arrayApplyCount += 1
    super.array_apply(xs, idx)
  }
  override def array_update(xs: AnyRef, idx: Int, value: Any) {
    arrayUpdateCount += 1
    super.array_update(xs, idx, value)
  }
}

class InstrumentedBoxes extends StandardBoxes {
  var booleanBoxCount   = 0
  var characterBoxCount = 0
  var byteBoxCount      = 0
  var shortBoxCount     = 0
  var integerBoxCount   = 0
  var longBoxCount      = 0
  var floatBoxCount     = 0
  var doubleBoxCount    = 0

  def printBoxedCounts() {
    val s = List(
      booleanBoxCount, characterBoxCount, byteBoxCount, shortBoxCount,
      integerBoxCount, longBoxCount, floatBoxCount, doubleBoxCount
    ) map (x => "%6s" format x) mkString ""

    Console.println(s)
  }

  override def boxToBoolean(x: Boolean): jl.Boolean = {
    booleanBoxCount += 1
    jl.Boolean.valueOf(x)
  }

  override def boxToCharacter(x: Char): jl.Character = {
    characterBoxCount += 1
    jl.Character.valueOf(x)
  }

  override def boxToByte(x: Byte): jl.Byte = {
    byteBoxCount += 1
    jl.Byte.valueOf(x)
  }

  override def boxToShort(x: Short): jl.Short = {
    shortBoxCount += 1
    jl.Short.valueOf(x)
  }

  override def boxToInteger(x: Int): jl.Integer = {
    integerBoxCount += 1
    jl.Integer.valueOf(x)
  }

  override def boxToLong(x: Long): jl.Long = {
    longBoxCount += 1
    jl.Long.valueOf(x)
  }

  override def boxToFloat(x: Float): jl.Float = {
    floatBoxCount += 1
    jl.Float.valueOf(x)
  }

  override def boxToDouble(x: Double): jl.Double = {
    doubleBoxCount += 1
    jl.Double.valueOf(x)
  }
}
