/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package mutable
package array

import scala.runtime.ScalaRunTime._
import scala.runtime.BoxedUnit
import scala.reflect.ClassTag

/** A class of `ArrayOps` for arrays of `Unit` types. */
class ofUnit(repr: Array[Unit]) extends ofRef(repr.asInstanceOf[Array[BoxedUnit]])

/** A class of `ArrayOps` for arrays containing reference types. */
class ofRef[T <: AnyRef](override val repr: Array[T]) extends ArrayOps[T] with ArrayLike[T, Array[T]] {

  override protected[this] def thisCollection: WrappedArray[T] = new WrappedArray.ofRef[T](repr)
  override protected[this] def toCollection(repr: Array[T]): WrappedArray[T] = new WrappedArray.ofRef[T](repr)
  override protected[this] def newBuilder = new ArrayBuilder.ofRef[T]()(ClassTag[T](arrayElementClass(repr.getClass)))
  override def foreach[U](f: T => U) { var i = 0 ; val len = length ; while (i < len) { f(this(i)); i += 1 } }

  def length: Int = repr.length
  def apply(index: Int): T = repr(index)
  def update(index: Int, elem: T) { repr(index) = elem }
}

/** A class of `ArrayOps` for arrays containing `byte`s. */
final class ofByte(override val repr: Array[Byte]) extends ArrayOps[Byte] with ArrayLike[Byte, Array[Byte]] {

  override protected[this] def thisCollection: WrappedArray[Byte] = new WrappedArray.ofByte(repr)
  override protected[this] def toCollection(repr: Array[Byte]): WrappedArray[Byte] = new WrappedArray.ofByte(repr)
  override protected[this] def newBuilder = new ArrayBuilder.ofByte
  override def foreach[U](f: Byte => U) { var i = 0 ; val len = length ; while (i < len) { f(this(i)); i += 1 } }

  def length: Int = repr.length
  def apply(index: Int): Byte = repr(index)
  def update(index: Int, elem: Byte) { repr(index) = elem }
}

/** A class of `ArrayOps` for arrays containing `short`s. */
final class ofShort(override val repr: Array[Short]) extends ArrayOps[Short] with ArrayLike[Short, Array[Short]] {

  override protected[this] def thisCollection: WrappedArray[Short] = new WrappedArray.ofShort(repr)
  override protected[this] def toCollection(repr: Array[Short]): WrappedArray[Short] = new WrappedArray.ofShort(repr)
  override protected[this] def newBuilder = new ArrayBuilder.ofShort
  override def foreach[U](f: Short => U) { var i = 0 ; val len = length ; while (i < len) { f(this(i)); i += 1 } }

  def length: Int = repr.length
  def apply(index: Int): Short = repr(index)
  def update(index: Int, elem: Short) { repr(index) = elem }
}

/** A class of `ArrayOps` for arrays containing `char`s. */
final class ofChar(override val repr: Array[Char]) extends ArrayOps[Char] with ArrayLike[Char, Array[Char]] {

  override protected[this] def thisCollection: WrappedArray[Char] = new WrappedArray.ofChar(repr)
  override protected[this] def toCollection(repr: Array[Char]): WrappedArray[Char] = new WrappedArray.ofChar(repr)
  override protected[this] def newBuilder = new ArrayBuilder.ofChar
  override def foreach[U](f: Char => U) { var i = 0 ; val len = length ; while (i < len) { f(this(i)); i += 1 } }

  def length: Int = repr.length
  def apply(index: Int): Char = repr(index)
  def update(index: Int, elem: Char) { repr(index) = elem }
}

/** A class of `ArrayOps` for arrays containing `int`s. */
final class ofInt(override val repr: Array[Int]) extends ArrayOps[Int] with ArrayLike[Int, Array[Int]] {

  override protected[this] def thisCollection: WrappedArray[Int] = new WrappedArray.ofInt(repr)
  override protected[this] def toCollection(repr: Array[Int]): WrappedArray[Int] = new WrappedArray.ofInt(repr)
  override protected[this] def newBuilder = new ArrayBuilder.ofInt
  override def foreach[U](f: Int => U) { var i = 0 ; val len = length ; while (i < len) { f(this(i)); i += 1 } }

  def length: Int = repr.length
  def apply(index: Int): Int = repr(index)
  def update(index: Int, elem: Int) { repr(index) = elem }
  def sum = thisCollection.sum
  def max = {
    if (repr.length == 0) thisCollection.max
    else {
      var max = repr(0)
      var i = 1
      while (i < repr.length) {
        if (repr(i) > max)
          max = repr(i)
        i += 1
      }
      max
    }
  }
}

/** A class of `ArrayOps` for arrays containing `long`s. */
final class ofLong(override val repr: Array[Long]) extends ArrayOps[Long] with ArrayLike[Long, Array[Long]] {

  override protected[this] def thisCollection: WrappedArray[Long] = new WrappedArray.ofLong(repr)
  override protected[this] def toCollection(repr: Array[Long]): WrappedArray[Long] = new WrappedArray.ofLong(repr)
  override protected[this] def newBuilder = new ArrayBuilder.ofLong
  override def foreach[U](f: Long => U) { var i = 0 ; val len = length ; while (i < len) { f(this(i)); i += 1 } }
  
  def length: Int = repr.length
  def apply(index: Int): Long = repr(index)
  def update(index: Int, elem: Long) { repr(index) = elem }
  def sum = thisCollection.sum
}

/** A class of `ArrayOps` for arrays containing `float`s. */
final class ofFloat(override val repr: Array[Float]) extends ArrayOps[Float] with ArrayLike[Float, Array[Float]] {

  override protected[this] def thisCollection: WrappedArray[Float] = new WrappedArray.ofFloat(repr)
  override protected[this] def toCollection(repr: Array[Float]): WrappedArray[Float] = new WrappedArray.ofFloat(repr)
  override protected[this] def newBuilder = new ArrayBuilder.ofFloat
  override def foreach[U](f: Float => U) { var i = 0 ; val len = length ; while (i < len) { f(this(i)); i += 1 } }

  def length: Int = repr.length
  def apply(index: Int): Float = repr(index)
  def update(index: Int, elem: Float) { repr(index) = elem }
}

/** A class of `ArrayOps` for arrays containing `double`s. */
final class ofDouble(override val repr: Array[Double]) extends ArrayOps[Double] with ArrayLike[Double, Array[Double]] {
  override protected[this] def thisCollection: WrappedArray[Double] = new WrappedArray.ofDouble(repr)
  override protected[this] def toCollection(repr: Array[Double]): WrappedArray[Double] = new WrappedArray.ofDouble(repr)
  override protected[this] def newBuilder = new ArrayBuilder.ofDouble
  override def foreach[U](f: Double => U) { var i = 0 ; val len = length ; while (i < len) { f(this(i)); i += 1 } }

  def length: Int = repr.length
  def apply(index: Int): Double = repr(index)
  def update(index: Int, elem: Double) { repr(index) = elem }
}

/** A class of `ArrayOps` for arrays containing `boolean`s. */
final class ofBoolean(override val repr: Array[Boolean]) extends ArrayOps[Boolean] with ArrayLike[Boolean, Array[Boolean]] {

  override protected[this] def thisCollection: WrappedArray[Boolean] = new WrappedArray.ofBoolean(repr)
  override protected[this] def toCollection(repr: Array[Boolean]): WrappedArray[Boolean] = new WrappedArray.ofBoolean(repr)
  override protected[this] def newBuilder = new ArrayBuilder.ofBoolean
  override def foreach[U](f: Boolean => U) { var i = 0 ; val len = length ; while (i < len) { f(this(i)); i += 1 } }

  def length: Int = repr.length
  def apply(index: Int): Boolean = repr(index)
  def update(index: Int, elem: Boolean) { repr(index) = elem }
}
