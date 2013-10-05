/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package foo

import scala.collection.{ mutable, immutable }
import java.{ lang => jl }

/** A common supertype for companion classes of primitive types.
 *  Should not be extended in user code.
 *
 *  A common trait for /companion/ objects of primitive types comes handy
 *  when parameterizing code on types. For instance, the specialized
 *  annotation is passed a sequence of types on which to specialize:
 *  {{{
 *     class Tuple1[@specialized(Unit, Int, Double) T]
 *  }}}
 */
trait PrimitiveCompanion extends Specializable {
  type Unboxed
  type Boxed <: AnyRef

  def isNumeric: Boolean
  def unboxedClass: Class[Unboxed]
  def boxedClass: Class[Boxed]
  def box(x: Unboxed): Boxed
  def unbox(x: Boxed): Unboxed
  def zero: Unboxed
}

object PrimitiveCompanion extends immutable.IndexedSeq[PrimitiveCompanion] {
  private final val companions = Array[PrimitiveCompanion](
    Boolean,
    Byte,
    Short,
    Char,
    Int,
    Long,
    Float,
    Double
  )
  def apply(idx: Int) = companions(idx)
  def length          = companions.length
}

object Boolean extends PrimitiveCompanion {
  type Unboxed             = Boolean
  type Boxed               = jl.Boolean
  def isNumeric            = false
  def unboxedClass         = classOf[jl.Boolean]
  def boxedClass           = classOf[Boolean]
  def zero                 = false
  def box(x: Boolean)      = jl.Boolean.valueOf(x)
  def unbox(x: jl.Boolean) = x.booleanValue
}

object Byte extends PrimitiveCompanion {
  type Unboxed          = Byte
  type Boxed            = jl.Byte
  def isNumeric         = true
  def unboxedClass      = classOf[Byte]
  def boxedClass        = classOf[jl.Byte]
  def zero              = (0: Byte)
  def box(x: Byte)      = jl.Byte.valueOf(x)
  def unbox(x: jl.Byte) = x.byteValue
}

object Short extends PrimitiveCompanion {
  type Unboxed           = Short
  type Boxed             = jl.Short
  def isNumeric          = true
  def unboxedClass       = classOf[Short]
  def boxedClass         = classOf[jl.Short]
  def zero               = (0: Short)
  def box(x: Short)      = jl.Short.valueOf(x)
  def unbox(x: jl.Short) = x.shortValue
}

object Char extends PrimitiveCompanion {
  type Unboxed               = Char
  type Boxed                 = jl.Character
  def isNumeric              = true
  def unboxedClass           = classOf[Char]
  def boxedClass             = classOf[jl.Character]
  def zero                   = (0: Char)
  def box(x: Char)           = jl.Character.valueOf(x)
  def unbox(x: jl.Character) = x.charValue
}

object Int extends PrimitiveCompanion {
  type Unboxed             = Int
  type Boxed               = jl.Integer
  def isNumeric            = true
  def unboxedClass         = classOf[Int]
  def boxedClass           = classOf[jl.Integer]
  def zero                 = 0
  def box(x: Int)          = jl.Integer.valueOf(x)
  def unbox(x: jl.Integer) = x.intValue
}

object Long extends PrimitiveCompanion {
  type Unboxed          = Long
  type Boxed            = jl.Long
  def isNumeric         = true
  def unboxedClass      = classOf[Long]
  def boxedClass        = classOf[jl.Long]
  def zero              = (0: Long)
  def box(x: Long)      = jl.Long.valueOf(x)
  def unbox(x: jl.Long) = x.longValue
}

object Float extends PrimitiveCompanion {
  type Unboxed           = Float
  type Boxed             = jl.Float
  def isNumeric          = true
  def unboxedClass       = classOf[Float]
  def boxedClass         = classOf[jl.Float]
  def zero               = (0: Float)
  def box(x: Float)      = jl.Float.valueOf(x)
  def unbox(x: jl.Float) = x.floatValue
}

object Double extends PrimitiveCompanion {
  type Unboxed            = Double
  type Boxed              = jl.Double
  def isNumeric           = true
  def unboxedClass        = classOf[Double]
  def boxedClass          = classOf[jl.Double]
  def zero                = (0: Double)
  def box(x: Double)      = jl.Double.valueOf(x)
  def unbox(x: jl.Double) = x.doubleValue
}
