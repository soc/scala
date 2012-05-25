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
  //
  // def foreach[U](f: A => U): Unit
  //
  // def hasDefiniteSize: Boolean
  //
  // /** The size of this $coll.
  //  *
  //  *  $willNotTerminateInf
  //  *
  //  *  @return    the number of elements in this $coll.
  //  */
  // def size: Int
  //
  // /** Tests whether the $coll is empty.
  //  *
  //  *  @return    `true` if the $coll contains no elements, `false` otherwise.
  //  */
  // def isEmpty: Boolean
  //
  // /** Tests whether the $coll is not empty.
  //  *
  //  *  @return    `true` if the $coll contains at least one element, `false` otherwise.
  //  */
  // def nonEmpty: Boolean

  /** Tests whether this $coll can be repeatedly traversed.  Always
   *  true for Iterables and false for Iterators unless overridden.
   *
   *  @return   `true` if it is repeatedly traversable, `false` otherwise.
   */
  def isIterableAgain: Boolean

  // /** Reduces the elements of this $coll using the specified associative binary operator.
  //  *
  //  *  $undefinedorder
  //  *
  //  *  @tparam A1      A type parameter for the binary operator, a supertype of `A`.
  //  *  @param op       A binary operator that must be associative.
  //  *  @return         The result of applying reduce operator `op` between all the elements if the $coll is nonempty.
  //  *  @throws UnsupportedOperationException
  //  *  if this $coll is empty.
  //  */
  // def reduce[A1 >: A](op: (A1, A1) => A1): A1
  //
  // /** Folds the elements of this $coll using the specified associative
  //  *  binary operator.
  //  *
  //  *  $undefinedorder
  //  *
  //  *  @tparam A1     a type parameter for the binary operator, a supertype of `A`.
  //  *  @param z       a neutral element for the fold operation; may be added to the result
  //  *                 an arbitrary number of times, and must not change the result (e.g., `Nil` for list concatenation,
  //  *                 0 for addition, or 1 for multiplication.)
  //  *  @param op      a binary operator that must be associative
  //  *  @return        the result of applying fold operator `op` between all the elements and `z`
  //  */
  // def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1
  //
  // /** Applies a binary operator to a start value and all elements of this $coll,
  //  *  going left to right.
  //  *
  //  *  Note: `/:` is alternate syntax for `foldLeft`; `z /: xs` is the same as
  //  *  `xs foldLeft z`.
  //  *
  //  *  Examples:
  //  *
  //  *  Note that the folding function used to compute b is equivalent to that used to compute c.
  //  *  {{{
  //  *      scala> val a = LinkedList(1,2,3,4)
  //  *      a: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2, 3, 4)
  //  *
  //  *      scala> val b = (5 /: a)(_+_)
  //  *      b: Int = 15
  //  *
  //  *      scala> val c = (5 /: a)((x,y) => x + y)
  //  *      c: Int = 15
  //  *  }}}
  //
  //  *  $willNotTerminateInf
  //  *  $orderDependentFold
  //  *
  //  *  @param   z    the start value.
  //  *  @param   op   the binary operator.
  //  *  @tparam  B    the result type of the binary operator.
  //  *  @return  the result of inserting `op` between consecutive elements of this $coll,
  //  *           going left to right with the start value `z` on the left:
  //  *           {{{
  //  *             op(...op(op(z, x_1), x_2), ..., x_n)
  //  *           }}}
  //  *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
  //  */
  // def /:[B](z: B)(op: (B, A) => B): B
  //
  // /** Applies a binary operator to all elements of this $coll and a start value,
  //  *  going right to left.
  //  *
  //  *  Note: `:\` is alternate syntax for `foldRight`; `xs :\ z` is the same as
  //  *  `xs foldRight z`.
  //  *  $willNotTerminateInf
  //  *  $orderDependentFold
  //  *
  //  *  Examples:
  //  *
  //  *  Note that the folding function used to compute b is equivalent to that used to compute c.
  //  *  {{{
  //  *      scala> val a = LinkedList(1,2,3,4)
  //  *      a: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2, 3, 4)
  //  *
  //  *      scala> val b = (a :\ 5)(_+_)
  //  *      b: Int = 15
  //  *
  //  *      scala> val c = (a :\ 5)((x,y) => x + y)
  //  *      c: Int = 15
  //  *
  //  *  }}}
  //  *
  //  *  @param   z    the start value
  //  *  @param   op   the binary operator
  //  *  @tparam  B    the result type of the binary operator.
  //  *  @return  the result of inserting `op` between consecutive elements of this $coll,
  //  *           going right to left with the start value `z` on the right:
  //  *           {{{
  //  *             op(x_1, op(x_2, ... op(x_n, z)...))
  //  *           }}}
  //  *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
  //  */
  // def :\[B](z: B)(op: (A, B) => B): B
  //
  // /** Applies a binary operator to a start value and all elements of this $coll,
  //  *  going left to right.
  //  *
  //  *  $willNotTerminateInf
  //  *  $orderDependentFold
  //  *
  //  *  @param   z    the start value.
  //  *  @param   op   the binary operator.
  //  *  @tparam  B    the result type of the binary operator.
  //  *  @return  the result of inserting `op` between consecutive elements of this $coll,
  //  *           going left to right with the start value `z` on the left:
  //  *           {{{
  //  *             op(...op(z, x_1), x_2, ..., x_n)
  //  *           }}}
  //  *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
  //  */
  // def foldLeft[B](z: B)(op: (B, A) => B): B
  //
  // /** Applies a binary operator to all elements of this $coll and a start value,
  //  *  going right to left.
  //  *
  //  *  $willNotTerminateInf
  //  *  $orderDependentFold
  //  *  @param   z    the start value.
  //  *  @param   op   the binary operator.
  //  *  @tparam  B    the result type of the binary operator.
  //  *  @return  the result of inserting `op` between consecutive elements of this $coll,
  //  *           going right to left with the start value `z` on the right:
  //  *           {{{
  //  *             op(x_1, op(x_2, ... op(x_n, z)...))
  //  *           }}}
  //  *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
  //  */
  // def foldRight[B](z: B)(op: (A, B) => B): B
  //
  // /** Aggregates the results of applying an operator to subsequent elements.
  //  *
  //  *  This is a more general form of `fold` and `reduce`. It has similar
  //  *  semantics, but does not require the result to be a supertype of the
  //  *  element type. It traverses the elements in different partitions
  //  *  sequentially, using `seqop` to update the result, and then applies
  //  *  `combop` to results from different partitions. The implementation of
  //  *  this operation may operate on an arbitrary number of collection
  //  *  partitions, so `combop` may be invoked an arbitrary number of times.
  //  *
  //  *  For example, one might want to process some elements and then produce
  //  *  a `Set`. In this case, `seqop` would process an element and append it
  //  *  to the list, while `combop` would concatenate two lists from different
  //  *  partitions together. The initial value `z` would be an empty set.
  //  *  {{{
  //  *    pc.aggregate(Set[Int]())(_ += process(_), _ ++ _)
  //  *  }}}
  //  *
  //  *  Another example is calculating geometric mean from a collection of doubles
  //  *  (one would typically require big doubles for this).
  //  *
  //  *  @tparam B        the type of accumulated results
  //  *  @param z         the initial value for the accumulated result of the partition - this
  //  *                   will typically be the neutral element for the `seqop` operator (e.g.
  //  *                   `Nil` for list concatenation or `0` for summation)
  //  *  @param seqop     an operator used to accumulate results within a partition
  //  *  @param combop    an associative operator used to combine results from different partitions
  //  */
  // def aggregate[B](z: B)(seqop: (B, A) => B, combop: (B, B) => B): B
  //
  // /** Applies a binary operator to all elements of this $coll, going right to left.
  //  *  $willNotTerminateInf
  //  *  $orderDependentFold
  //  *
  //  *  @param  op    the binary operator.
  //  *  @tparam  B    the result type of the binary operator.
  //  *  @return  the result of inserting `op` between consecutive elements of this $coll,
  //  *           going right to left:
  //  *           {{{
  //  *             op(x_1, op(x_2, ..., op(x_{n-1}, x_n)...))
  //  *           }}}
  //  *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
  //  *  @throws `UnsupportedOperationException` if this $coll is empty.
  //  */
  // def reduceRight[B >: A](op: (A, B) => B): B
  //
  // /** Counts the number of elements in the $coll which satisfy a predicate.
  //  *
  //  *  @param p     the predicate  used to test elements.
  //  *  @return      the number of elements satisfying the predicate `p`.
  //  */
  // def count(p: A => Boolean): Int
  //
  // def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A
  //
  // def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A
  //
  // def forall(pred: A => Boolean): Boolean
  //
  // def exists(pred: A => Boolean): Boolean
  //
  // /** Finds the first element of the $coll satisfying a predicate, if any.
  //  *
  //  *  $mayNotTerminateInf
  //  *  $orderDependent
  //  *
  //  *  @param pred    the predicate used to test elements.
  //  *  @return        an option value containing the first element in the $coll
  //  *                 that satisfies `p`, or `None` if none exists.
  //  */
  // def find(pred: A => Boolean): Option[A]
  //
  // /** Copies values of this $coll to an array.
  //  *  Fills the given array `xs` with values of this $coll.
  //  *  Copying will stop once either the end of the current $coll is reached,
  //  *  or the end of the array is reached.
  //  *
  //  *  @param  xs     the array to fill.
  //  *  @tparam B      the type of the elements of the array.
  //  *
  //  *  @usecase def copyToArray(xs: Array[A]): Unit
  //  *    @inheritdoc
  //  *
  //  *    $willNotTerminateInf
  //  */
  // def copyToArray[B >: A](xs: Array[B]): Unit
  //
  // /** Copies values of this $coll to an array.
  //  *  Fills the given array `xs` with values of this $coll, beginning at index `start`.
  //  *  Copying will stop once either the end of the current $coll is reached,
  //  *  or the end of the array is reached.
  //  *
  //  *  @param  xs     the array to fill.
  //  *  @param  start  the starting index.
  //  *  @tparam B      the type of the elements of the array.
  //  *
  //  *  @usecase def copyToArray(xs: Array[A], start: Int): Unit
  //  *    @inheritdoc
  //  *
  //  *    $willNotTerminateInf
  //  */
  // def copyToArray[B >: A](xs: Array[B], start: Int): Unit
  //
  // def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit
  //
  // /** Displays all elements of this $coll in a string using start, end, and
  //  *  separator strings.
  //  *
  //  *  @param start the starting string.
  //  *  @param sep   the separator string.
  //  *  @param end   the ending string.
  //  *  @return      a string representation of this $coll. The resulting string
  //  *               begins with the string `start` and ends with the string
  //  *               `end`. Inside, the string representations (w.r.t. the method
  //  *               `toString`) of all elements of this $coll are separated by
  //  *               the string `sep`.
  //  *
  //  *  @example  `List(1, 2, 3).mkString("(", "; ", ")") = "(1; 2; 3)"`
  //  */
  // def mkString(start: String, sep: String, end: String): String
  //
  // /** Displays all elements of this $coll in a string using a separator string.
  //  *
  //  *  @param sep   the separator string.
  //  *  @return      a string representation of this $coll. In the resulting string
  //  *               the string representations (w.r.t. the method `toString`)
  //  *               of all elements of this $coll are separated by the string `sep`.
  //  *
  //  *  @example  `List(1, 2, 3).mkString("|") = "1|2|3"`
  //  */
  // def mkString(sep: String): String
  //
  // /** Displays all elements of this $coll in a string.
  //  *
  //  *  @return a string representation of this $coll. In the resulting string
  //  *          the string representations (w.r.t. the method `toString`)
  //  *          of all elements of this $coll follow each other without any
  //  *          separator string.
  //  */
  // def mkString: String
  //
  // /** Converts this $coll to an array.
  //  *
  //  *  @tparam A1 the type of the elements of the array. An `ArrayTag` for
  //  *             this type must be available.
  //  *  @return    an array containing all elements of this $coll.
  //  *
  //  *  @usecase def toArray: Array[A]
  //  *    @inheritdoc
  //  *
  //  *    $willNotTerminateInf
  //  *
  //  *    @return  an array containing all elements of this $coll.
  //  *             An `ArrayTag` must be available for the element type of this $coll.
  //  */
  // def toArray[A1 >: A: ArrayTag]: Array[A1]
  //
  // /** Converts this $coll to a list.
  //  *  $willNotTerminateInf
  //  *  @return a list containing all elements of this $coll.
  //  */
  // def toList: List[A]
  //
  // /** Converts this $coll to an indexed sequence.
  //  *  $willNotTerminateInf
  //  *  @return an indexed sequence containing all elements of this $coll.
  //  */
  // def toIndexedSeq: immutable.IndexedSeq[A]

  /** Returns an Iterator over the elements in this $coll.  Will return
   *  the same Iterator if this instance is already an Iterator.
   *  $willNotTerminateInf
   *  @return an Iterator containing all elements of this $coll.
   */
  def toIterator: Iterator[A]

  // /** Converts this $coll to a mutable buffer.
  //  *  $willNotTerminateInf
  //  *  @return a buffer containing all elements of this $coll.
  //  */
  // def toBuffer[A1 >: A]: collection.mutable.Buffer[A1]
  //
  // /** Converts this $coll to an unspecified Iterable.  Will return
  //  *  the same collection if this instance is already Iterable.
  //  *  $willNotTerminateInf
  //  *  @return a Iterable containing all elements of this $coll.
  //  */
  // def toIterable: Iterable[A]
  //
  // /** Converts this $coll to an iterable collection.  Note that
  //  *  the choice of target `Iterable` is lazy in this default implementation
  //  *  as this `IterableOnce` may be lazy and unevaluated (i.e. it may
  //  *  be an iterator which is only traversable once).
  //  *
  //  *  $willNotTerminateInf
  //  *  @return an `Iterable` containing all elements of this $coll.
  //  */
  // def toIterable: Iterable[A]
  //
  // /** Converts this $coll to a sequence. As with `toIterable`, it's lazy
  //  *  in this default implementation, as this `IterableOnce` may be
  //  *  lazy and unevaluated.
  //  *
  //  *  $willNotTerminateInf
  //  *  @return a sequence containing all elements of this $coll.
  //  */
  // def toSeq: Seq[A]
  //
  // /** Converts this $coll to a set.
  //  *  $willNotTerminateInf
  //  *  @return      a set containing all elements of this $coll.
  //  */
  // def toSet[A1 >: A]: Set[A1]
  //
  // /** Converts this $coll to a map.  This method is unavailable unless
  //  *  the elements are members of Tuple2, each ((T, U)) becoming a key-value
  //  *  pair in the map.  Duplicate keys will be overwritten by later keys:
  //  *  if this is an unordered collection, which key is in the resulting map
  //  *  is undefined.
  //  *  @return    a map containing all elements of this $coll.
  //  *
  //  *  @usecase   def toMap[T, U]: Map[T, U]
  //  *    @inheritdoc
  //  *    $willNotTerminateInf
  //  *    @return    a map of type `immutable.Map[T, U]`
  //  *               containing all key/value pairs of type `(T, U)` of this $coll.
  //  */
  // def toMap[K, V](implicit ev: A <:< (K, V)): Map[K, V]

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
  //
  // def sum[B >: A](implicit num: Numeric[B]): B = foldLeft(num.zero)(num.plus)
  //
  // def product[B >: A](implicit num: Numeric[B]): B = foldLeft(num.one)(num.times)
  //
  // def min[B >: A](implicit cmp: Ordering[B]): A = {
  //   if (isEmpty)
  //     throw new UnsupportedOperationException("empty.min")
  //
  //   reduceLeft((x, y) => if (cmp.lteq(x, y)) x else y)
  // }
  //
  // def max[B >: A](implicit cmp: Ordering[B]): A = {
  //   if (isEmpty)
  //     throw new UnsupportedOperationException("empty.max")
  //
  //   reduceLeft((x, y) => if (cmp.gteq(x, y)) x else y)
  // }
  //
  // def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
  //   if (isEmpty)
  //     throw new UnsupportedOperationException("empty.maxBy")
  //
  //   reduceLeft((x, y) => if (cmp.gteq(f(x), f(y))) x else y)
  // }
  // def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
  //   if (isEmpty)
  //     throw new UnsupportedOperationException("empty.minBy")
  //
  //   reduceLeft((x, y) => if (cmp.lteq(f(x), f(y))) x else y)
  // }

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

  def toIterable: Iterable[A]

  def toList: List[A] = (new ListBuffer[A] ++= this).toList
  // 
  // def toIterable: Iterable[A] = Iterable[A]() ++ this

  def toSeq: Seq[A] = Seq[A]() ++ this

  def toIndexedSeq: immutable.IndexedSeq[A] = immutable.IndexedSeq() ++ this

  def toBuffer[B >: A]: mutable.Buffer[B] = new ArrayBuffer[B] ++= this

  def toSet[B >: A]: immutable.Set[B] = immutable.Set() ++ this

  def toMap[T, U](implicit ev: A <:< (T, U)): immutable.Map[T, U] = {
    val b = immutable.Map.newBuilder[T, U]
    for (x <- self)
      b += x

    b.result
  }

  def mkString(start: String, sep: String, end: String): String =
    addString(new StringBuilder(), start, sep, end).toString

  def mkString(sep: String): String = mkString("", sep, "")

  def mkString: String = mkString("")

  /** Appends all elements of this $coll to a string builder using start, end, and separator strings.
   *  The written text begins with the string `start` and ends with the string `end`.
   *  Inside, the string representations (w.r.t. the method `toString`)
   *  of all elements of this $coll are separated by the string `sep`.
   *
   * Example:
   *
   * {{{
   *      scala> val a = LinkedList(1,2,3,4)
   *      a: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2, 3, 4)
   *
   *      scala> val b = new StringBuilder()
   *      b: StringBuilder =
   *
   *      scala> a.addString(b, "LinkedList(", ", ", ")")
   *      res1: StringBuilder = LinkedList(1, 2, 3, 4)
   * }}}
   *
   *  @param  b    the string builder to which elements are appended.
   *  @param start the starting string.
   *  @param sep   the separator string.
   *  @param end   the ending string.
   *  @return      the string builder `b` to which elements were appended.
   */
  def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    var first = true

    b append start
    for (x <- self) {
      if (first) {
        b append x
        first = false
      }
      else {
        b append sep
        b append x
      }
    }
    b append end

    b
  }

  /** Appends all elements of this $coll to a string builder using a separator string.
   *  The written text consists of the string representations (w.r.t. the method `toString`)
   *  of all elements of this $coll, separated by the string `sep`.
   *
   * Example:
   *
   * {{{
   *      scala> val a = LinkedList(1,2,3,4)
   *      a: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2, 3, 4)
   *
   *      scala> val b = new StringBuilder()
   *      b: StringBuilder =
   *
   *      scala> a.addString(b, ", ")
   *      res0: StringBuilder = 1, 2, 3, 4
   * }}}
   *
   *  @param  b    the string builder to which elements are appended.
   *  @param sep   the separator string.
   *  @return      the string builder `b` to which elements were appended.
   */
  def addString(b: StringBuilder, sep: String): StringBuilder = addString(b, "", sep, "")

  /** Appends all elements of this $coll to a string builder.
   *  The written text consists of the string representations (w.r.t. the method
   * `toString`) of all elements of this $coll without any separator string.
   *
   * Example:
   *
   * {{{
   *      scala> val a = LinkedList(1,2,3,4)
   *      a: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2, 3, 4)
   *
   *      scala> val b = new StringBuilder()
   *      b: StringBuilder =
   *
   *      scala> val h = a.addString(b)
   *      b: StringBuilder = 1234
   * }}}

   *  @param  b    the string builder to which elements are appended.
   *  @return      the string builder `b` to which elements were appended.
   */
  def addString(b: StringBuilder): StringBuilder = addString(b, "")
}


object IterableOnce {
  implicit def orderedIterableOnce[A](xs: IterableOnce[A]) = new OrderOps(xs)
  implicit def numericIterableOnce[A](xs: IterableOnce[A]) = new NumOps(xs)
  // implicit def alternateImplicit[A](trav: IterableOnce[A]) = new ForceImplicitAmbiguity
  implicit def flattenIterableOnce[A, CC[_]](travs: IterableOnce[CC[A]])(implicit ev: CC[A] => IterableOnce[A]) =
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

  class FlattenOps[A](travs: IterableOnce[IterableOnce[A]]) {
    def flatten: Iterator[A] = new AbstractIterator[A] {
      val its = travs.toIterator
      private var it: Iterator[A] = Iterator.empty
      def hasNext: Boolean = it.hasNext || its.hasNext && { it = its.next.toIterator; hasNext }
      def next(): A = if (hasNext) it.next() else Iterator.empty.next()
    }
  }
  // 
  // class ForceImplicitAmbiguity

  implicit class MonadOps[+A](trav: IterableOnce[A]) {
    def map[B](f: A => B): IterableOnce[B] = trav.toIterator map f
    def flatMap[B](f: A => IterableOnce[B]): IterableOnce[B] = trav.toIterator flatMap f
    def withFilter(p: A => Boolean) = trav.toIterator filter p
    def filter(p: A => Boolean): IterableOnce[A] = withFilter(p)
  }
}
