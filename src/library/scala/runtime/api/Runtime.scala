/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package runtime
package api

import scala.{ collection => coll }
import java.lang.{ Class => jClass }
import java.lang.reflect.{ Method => jMethod }

trait EqualityRuntime {
  def isSameProduct(x: Product, y: Product): Boolean
  def isSameSeq(xs1: coll.Seq[Any], xs2: coll.Seq[Any]): Boolean

  def hash(x: Any): Int
  def hash(x: Product): Int
  def hash(x: Boolean): Int
  def hash(x: Byte): Int
  def hash(x: Char): Int
  def hash(x: Short): Int
  def hash(x: Int): Int
  def hash(lv: Long): Int
  def hash(fv: Float): Int
  def hash(dv: Double): Int
}

trait ArrayRuntime {
  def isArray(x: Any, atLevel: Int): Boolean
  def toArray(xs: coll.Seq[Any]): Array[AnyRef]
  def toObjectArray(src: AnyRef): Array[AnyRef]
  def arrayClass(clazz: jClass[_]): jClass[_]
  def arrayElementClass(schematic: Any): jClass[_]
  def arrayApply(xs: AnyRef, idx: Int): Any
  def arrayClone(xs: AnyRef): AnyRef
  def arrayLength(xs: AnyRef): Int
  def arrayUpdate(xs: AnyRef, idx: Int, value: Any): Unit
}

trait Runtime extends EqualityRuntime with ArrayRuntime {
  def stringOf(arg: Any): String
  def stringOf(arg: Any, maxElements: Int): String

  def ensureAccessible(m: jMethod): jMethod
  def boxedClass(clazz: jClass[_]): jClass[_]
  def typedIterator[T](x: Product): coll.Iterator[T]
}
