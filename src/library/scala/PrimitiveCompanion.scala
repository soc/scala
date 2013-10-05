/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

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
  def iterator        = companions.iterator
}

object Boolean extends PrimitiveCompanion {
  type Unboxed             = Boolean
  type Boxed               = jl.Boolean
  def zero                 = false
  def box(x: Boolean)      = Boolean.valueOf(x)
  def unbox(x: jl.Boolean) = x.booleanValue
}





