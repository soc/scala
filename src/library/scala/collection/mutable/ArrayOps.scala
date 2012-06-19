/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package mutable

import compat.Platform.arraycopy
import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime._

/** This class serves as a wrapper for `Array`s with all the operations found in
 *  indexed sequences. Where needed, instances of arrays are implicitly converted
 *  into this class.
 *
 *  The difference between this class and `WrappedArray` is that calling transformer
 *  methods such as `filter` and `map` will yield an array, whereas a `WrappedArray`
 *  will remain a `WrappedArray`.
 *
 *  @since 2.8
 *
 *  @tparam T   type of the elements contained in this array.
 *
 *  @define Coll `ArrayOps`
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
abstract class ArrayOps[T] extends ArrayLike[T, Array[T]] {

  private def elementClass: Class[_] =
    arrayElementClass(repr.getClass)

  override def copyToArray[U >: T](xs: Array[U], start: Int, len: Int) {
    var l = math.min(len, repr.length)
    if (xs.length - start < l) l = xs.length - start max 0
    Array.copy(repr, 0, xs, start, l)
  }

  override def toArray[U >: T : ClassTag]: Array[U] = {
    val thatElementClass = arrayElementClass(implicitly[ClassTag[U]])
    if (elementClass eq thatElementClass)
      repr.asInstanceOf[Array[U]]
    else
      super.toArray[U]
  }

  /** Flattens a two-dimensional array by concatenating all its rows
   *  into a single array.
   *
   *  @tparam U        Type of row elements.
   *  @param asTrav    A function that converts elements of this array to rows - arrays of type `U`.
   *  @return          An array obtained by concatenating rows of this array.
   */
  def flatten[U](implicit asTrav: T => collection.Iterable[U], m: ClassTag[U]): Array[U] = {
    val b = Array.newBuilder[U]
    b sizeHint (thisCollection map (_.size) sum)
    this foreach (b ++= _)
    b.result
  }

  /** Transposes a two dimensional array.
   *
   *  @tparam U       Type of row elements.
   *  @param asArray  A function that converts elements of this array to rows - arrays of type `U`.
   *  @return         An array obtained by replacing elements of this arrays with rows the represent.
   */
  def transpose[U](implicit asArray: T => Array[U]): Array[Array[U]] = {
    def mkRowBuilder() = Array.newBuilder(ClassTag[U](arrayElementClass(elementClass)))
    val bs = asArray(head) map (_ => mkRowBuilder())
    for (xs <- this) {
      var i = 0
      for (x <- asArray(xs)) {
        bs(i) += x
        i += 1
      }
    }
    val bb: Builder[Array[U], Array[Array[U]]] = Array.newBuilder(ClassTag[Array[U]](elementClass))
    for (b <- bs) bb += b.result
    bb.result
  }
}

/**
 * A companion object for `ArrayOps`.
 *
 * @since 2.8
 */
object ArrayOps { }
