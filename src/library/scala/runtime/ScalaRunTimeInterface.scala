/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

import scala.collection.{ Iterator, TraversableOnce }
import java.lang.{ Class => jClass }
import java.lang.reflect.{ Method => JMethod }

abstract class ScalaRunTimeInterface {
  // Arrays
  def isArray(x: Any, atLevel: Int): Boolean
  def toArray[T](xs: collection.Seq[T]): Array[AnyRef]
  def toObjectArray(src: AnyRef): Array[Object]
  def arrayClass(clazz: jClass[_]): jClass[_]
  def arrayElementClass(schematic: Any): jClass[_]
  def array_apply(xs: AnyRef, idx: Int): Any
  def array_clone(xs: AnyRef): AnyRef
  def array_length(xs: AnyRef): Int
  def array_update(xs: AnyRef, idx: Int, value: Any): Unit

  // Any
  def _equals(x: Product, y: Any): Boolean
  def _hashCode(x: Product): Int
  def _toString(x: Product): String

  // AnyVal
  def box[T](clazz: jClass[T]): jClass[_]
  def isAnyVal(x: Any): Boolean
  def isValueClass(clazz: Class[_]): Boolean
  def hash(dv: Double): Int
  def hash(fv: Float): Int
  def hash(lv: Long): Int
  def hash(x: Any): Int
  def hash(x: Boolean): Int
  def hash(x: Byte): Int
  def hash(x: Char): Int
  def hash(x: Int): Int
  def hash(x: Number): Int
  def hash(x: Short): Int
  def hash(x: Unit): Int

  // Products / Collections
  def isTuple(x: Any): Boolean
  def sameElements(xs1: collection.Seq[Any], xs2: collection.Seq[Any]): Boolean
  def typedProductIterator[T](x: Product): Iterator[T]

  // Strings
  def replStringOf(arg: Any, maxElements: Int): String
  def stringOf(arg: Any): String
  def stringOf(arg: Any, maxElements: Int): String

  // Helpers
  def checkInitialized[T <: AnyRef](x: T): T
  def ensureAccessible(m: JMethod): JMethod
  def inlinedEquals(x: Object, y: Object): Boolean

  // Misc
  private[scala] def checkZip(what: String, coll1: TraversableOnce[_], coll2: TraversableOnce[_]): Unit
}
