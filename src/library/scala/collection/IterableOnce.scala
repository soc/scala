/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

import mutable.{ Buffer, Builder, ListBuffer, ArrayBuffer }
import annotation.unchecked.{ uncheckedVariance => uV }
import language.{implicitConversions, higherKinds}

/** A template trait for collections which can be traversed either once only
 *  or one or more times.
 *  $traversableonceinfo
 *
 *  @author Martin Odersky
 *  @author Paul Phillips
 *  @version 2.8
 *  @since   2.8
 *
 *  @define coll traversable or iterator
 *
 *  @tparam A    the element type of the collection
 *
 *  @define traversableonceinfo
 *  This trait exists primarily to eliminate code duplication between
 *  `Iterator` and `Iterable`, and thus implements some of the common
 *  methods that can be implemented solely in terms of foreach without
 *  access to a `Builder`. It also includes a number of abstract methods
 *  whose implementations are provided by `Iterator`, `Iterable`, etc.
 *  It contains implementations common to `Iterators` and
 *  `Iterables`, such as folds, conversions, and other operations which
 *  traverse some or all of the elements and return a derived value.
 *  Directly subclassing `IterableOnce` is not recommended - instead,
 *  consider declaring an `Iterator` with a `next` and `hasNext` method,
 *  creating an `Iterator` with one of the methods on the `Iterator` object,
 *  or declaring a subclass of `Iterable`.
 *
 *  @define coll traversable or iterator
 *  @define orderDependent
 *
 *    Note: might return different results for different runs, unless the underlying collection type is ordered.
 *  @define orderDependentFold
 *
 *    Note: might return different results for different runs, unless the
 *    underlying collection type is ordered or the operator is associative
 *    and commutative.
 *  @define mayNotTerminateInf
 *
 *    Note: may not terminate for infinite-sized collections.
 *  @define willNotTerminateInf
 *
 *    Note: will not terminate for infinite-sized collections.
 */
trait IterableOnce[+A] extends Any {
  self =>

  /** Tests whether this $coll can be repeatedly traversed.  Always
   *  true for Iterables and false for Iterators unless overridden.
   *
   *  @return   `true` if it is repeatedly traversable, `false` otherwise.
   */
  def isIterableAgain: Boolean

  /** Returns an Iterator over the elements in this $coll.  Will return
   *  the same Iterator if this instance is already an Iterator.
   *  $willNotTerminateInf
   *  @return an Iterator containing all elements of this $coll.
   */
  def toIterator: Iterator[A]

  /** Self-documenting abstract methods. */
  def foreach[U](f: A => U): Unit
  def isEmpty: Boolean
  def hasDefiniteSize: Boolean

  /** Presently these are abstract because the Iterable versions use
   *  breakable/break, and I wasn't sure enough of how that's supposed to
   *  function to consolidate them with the Iterator versions.
   */
  def forall(p: A => Boolean): Boolean
  def exists(p: A => Boolean): Boolean
  def find(p: A => Boolean): Option[A]
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit

  // for internal use
  protected[this] def reversed = {
    var elems: List[A] = Nil
    self foreach (elems ::= _)
    elems
  }

  def size: Int = {
    var result = 0
    for (x <- self) result += 1
    result
  }

  def nonEmpty: Boolean = !isEmpty

  def count(p: A => Boolean): Int = {
    var cnt = 0
    for (x <- this)
      if (p(x)) cnt += 1

    cnt
  }

  /** Finds the first element of the $coll for which the given partial
   *  function is defined, and applies the partial function to it.
   *
   *  $mayNotTerminateInf
   *  $orderDependent
   *
   *  @param pf   the partial function
   *  @return     an option value containing pf applied to the first
   *              value for which it is defined, or `None` if none exists.
   *  @example    `Seq("a", 1, 5L).collectFirst({ case x: Int => x*10 }) = Some(10)`
   */
  def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = {
    for (x <- self.toIterator) { // make sure to use an iterator or `seq`
      if (pf isDefinedAt x)
        return Some(pf(x))
    }
    None
  }

  def /:[B](z: B)(op: (B, A) => B): B = foldLeft(z)(op)

  def :\[B](z: B)(op: (A, B) => B): B = foldRight(z)(op)

  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var result = z
    this foreach (x => result = op(result, x))
    result
  }

  def foldRight[B](z: B)(op: (A, B) => B): B =
    reversed.foldLeft(z)((x, y) => op(y, x))

  def reduceLeft[B >: A](op: (B, A) => B): B = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.reduceLeft")

    var first = true
    var acc: B = 0.asInstanceOf[B]

    for (x <- self) {
      if (first) {
        acc = x
        first = false
      }
      else acc = op(acc, x)
    }
    acc
  }

  def reduceRight[B >: A](op: (A, B) => B): B = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.reduceRight")

    reversed.reduceLeft[B]((x, y) => op(y, x))
  }

  def reduce[A1 >: A](op: (A1, A1) => A1): A1 = reduceLeft(op)

  def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1 = foldLeft(z)(op)

  def aggregate[B](z: B)(seqop: (B, A) => B, combop: (B, B) => B): B = foldLeft(z)(seqop)

  /** Copies all elements of this $coll to a buffer.
   *  $willNotTerminateInf
   *  @param  dest   The buffer to which elements are copied.
   */
  def copyToBuffer[B >: A](dest: Buffer[B]): Unit = dest ++= this

  def copyToArray[B >: A](xs: Array[B], start: Int): Unit =
    copyToArray(xs, start, xs.length - start)

  def copyToArray[B >: A](xs: Array[B]): Unit =
    copyToArray(xs, 0, xs.length)

  def toArray[B >: A : ArrayTag]: Array[B] = {
    if (isIterableAgain) {
      val result = new Array[B](size)
      copyToArray(result, 0)
      result
    }
    else toBuffer.toArray
  }

  def toList: List[A] = (new ListBuffer[A] ++= this).toList

  def toSeq: Seq[A] = Seq[A]() ++ this

  def toIndexedSeq: immutable.IndexedSeq[A] = immutable.IndexedSeq() ++ this

  def toBuffer[B >: A]: mutable.Buffer[B] = new ArrayBuffer[B] ++= this

  def toMap[T, U](implicit ev: A <:< (T, U)): immutable.Map[T, U] = {
    val b = immutable.Map.newBuilder[T, U]
    for (x <- self)
      b += x

    b.result
  }

  def mkString: String = mkString("")
  def mkString(sep: String): String = {
    if (isEmpty) ""
    else {
      val it = toIterator
      val sb = new StringBuilder
      sb append it.next
      while (it.hasNext) {
        if (sep != "")
          sb append sep
        sb append it.next
      }
      sb.toString
    }
  }
  def mkString(start: String, sep: String, end: String): String = start + mkString(sep) + end
}


object IterableOnce {
  implicit def orderedIterableOnce[A](xs: IterableOnce[A]) = new OrderOps(xs)
  implicit def numericIterableOnce[A](xs: IterableOnce[A]) = new NumOps(xs)
  implicit def flattenIterableOnce[A, CC[_]](travs: IterableOnce[CC[A]])(implicit ev: CC[A] => IterableOnce[A]): FlattenOps[A] =
    new FlattenOps[A](travs map ev)
  
  /* Functionality reused in Iterator.CanBuildFrom */
  private[collection] abstract class BufferedCanBuildFrom[A, Coll[X] <: IterableOnce[X]] extends generic.CanBuildFrom[Coll[_], A, Coll[A]] {
    def bufferToColl[B](buff: ArrayBuffer[B]): Coll[B]
    def traversableToColl[B](t: Iterable[B]): Coll[B]
    
    def newIterator: Builder[A, Coll[A]] = new ArrayBuffer[A] mapResult bufferToColl

    /** Creates a new builder on request of a collection.
     *  @param from  the collection requesting the builder to be created.
     *  @return the result of invoking the `genericBuilder` method on `from`.
     */
    def apply(from: Coll[_]): Builder[A, Coll[A]] = from match {
      case xs: generic.GenericIterableTemplate[_, _] => xs.genericBuilder.asInstanceOf[Builder[A, Iterable[A]]] mapResult {
        case res => traversableToColl(res.asInstanceOf[Iterable[A]])
      }
      case _ => newIterator
    }

    /** Creates a new builder from scratch
     *  @return the result of invoking the `newBuilder` method of this factory.
     */
    def apply() = newIterator
  }
  
  /** With the advent of `IterableOnce`, it can be useful to have a builder which
   *  operates on `Iterator`s so they can be treated uniformly along with the collections.
   *  See `scala.util.Random.shuffle` or `scala.concurrent.Future.sequence` for an example.
   */
  class OnceCanBuildFrom[A] extends BufferedCanBuildFrom[A, IterableOnce] {
    def bufferToColl[B](buff: ArrayBuffer[B]) = buff.iterator
    def traversableToColl[B](t: Iterable[B]) = t
  }
  
  /** Evidence for building collections from `IterableOnce` collections */
  implicit def OnceCanBuildFrom[A] = new OnceCanBuildFrom[A]
  
  final class NumOps[A](val xs: IterableOnce[A]) extends AnyVal {
    /** Sums up the elements of this collection.
     *
     *   @param   num  an implicit parameter defining a set of numeric operations
     *                 which includes the `+` operator to be used in forming the sum.
     *   @tparam  A1   the result type of the `+` operator.
     *   @return       the sum of all elements of this $coll with respect to the `+` operator in `num`.
     *
     *   @usecase def sum: A
     *     @inheritdoc
     *
     *     @return       the sum of all elements in this $coll of numbers of type `Int`.
     *     Instead of `Int`, any other type `T` with an implicit `Numeric[T]` implementation
     *     can be used as element type of the $coll and as result type of `sum`.
     *     Examples of such types are: `Long`, `Float`, `Double`, `BigInt`.
     *
     */
    def sum(implicit num: Numeric[A]) = xs.foldLeft(num.zero)(num.plus)

    /** Multiplies up the elements of this collection.
     *
     *   @param   num  an implicit parameter defining a set of numeric operations
     *                 which includes the `*` operator to be used in forming the product.
     *   @tparam  A1   the result type of the `*` operator.
     *   @return       the product of all elements of this $coll with respect to the `*` operator in `num`.
     *
     *   @usecase def product: A
     *     @inheritdoc
     *
     *     @return       the product of all elements in this $coll of numbers of type `Int`.
     *     Instead of `Int`, any other type `T` with an implicit `Numeric[T]` implementation
     *     can be used as element type of the $coll and as result type of `product`.
     *     Examples of such types are: `Long`, `Float`, `Double`, `BigInt`.
     */
    def product(implicit num: Numeric[A]) = xs.foldLeft(num.one)(num.times)
  }
  final class OrderOps[A](val xs: IterableOnce[A]) extends AnyVal {
    /** Finds the smallest element.
     *
     *  @param    ord   An ordering to be used for comparing elements.
     *  @tparam   A1    The type over which the ordering is defined.
     *  @return   the smallest element of this $coll with respect to the ordering `cmp`.
     *
     *  @usecase def min: A
     *    @inheritdoc
     *
     *    @return   the smallest element of this $coll
     */
    def min(implicit ord: Ordering[A]) = (
      if (xs.isEmpty) throw new UnsupportedOperationException("empty.min")
      else xs.reduceLeft((x, y) => if (ord.lteq(x, y)) x else y)
    )
    /** Finds the largest element.
     *
     *  @param    ord   An ordering to be used for comparing elements.
     *  @tparam   A1    The type over which the ordering is defined.
     *  @return   the largest element of this $coll with respect to the ordering `cmp`.
     *
     *  @usecase def max: A
     *    @inheritdoc
     *
     *    @return   the largest element of this $coll.
     */
    def max(implicit ord: Ordering[A]) = (
      if (xs.isEmpty) throw new UnsupportedOperationException("empty.min")
      else xs.reduceLeft((x, y) => if (ord.gteq(x, y)) x else y)
    )
    def minBy[B](f: A => B)(implicit ord: Ordering[B]): A = (
      if (xs.isEmpty) throw new UnsupportedOperationException("empty.minBy")
      else xs.reduceLeft((x, y) => if (ord.lteq(f(x), f(y))) x else y)
    )
    def maxBy[B](f: A => B)(implicit ord: Ordering[B]): A = (
      if (xs.isEmpty) throw new UnsupportedOperationException("empty.maxBy")
      else xs.reduceLeft((x, y) => if (ord.gteq(f(x), f(y))) x else y)
    )
  }

  class FlattenOps[A](val travs: IterableOnce[IterableOnce[A]]) {

    def flatten: Iterator[A] = new AbstractIterator[A] {
      val its = travs.toIterator
      private var it: Iterator[A] = Iterator.empty
      def hasNext: Boolean = it.hasNext || its.hasNext && { it = its.next.toIterator; hasNext }
      def next(): A = if (hasNext) it.next() else Iterator.empty.next()
    }
  }
  
  implicit final class InvariantOps[A](val trav: IterableOnce[A]) extends AnyVal {
    def toSet[A1 >: A]: immutable.Set[A1] = immutable.Set[A1]() ++ trav
  }

  /** That's the actual IterableOnce map. */
  implicit final class MonadOps[A](val trav: IterableOnce[A]) extends AnyVal {
    def map[B](f: A => B): IterableOnce[B] = trav.toIterator map f
    def flatMap[B](f: A => IterableOnce[B]): IterableOnce[B] = trav.toIterator flatMap f
    def withFilter(p: A => Boolean) = trav.toIterator filter p
    def filter(p: A => Boolean): IterableOnce[A] = withFilter(p)
  }
}
