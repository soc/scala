/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

import generic._
import immutable.List
import mutable.Builder
import annotation.unchecked.{ uncheckedVariance => uV }

/** A template trait for iterable collections of type `Iterable[A]`.
 *  $iterableInfo
 *  @define iterableInfo
 *    This is a base trait for all $mutability Scala collections that define an `iterator`
 *    method to step through one-by-one the collection's elements.
 *    Implementations of this trait need to provide a concrete method with
 *    signature:
 *    {{{
 *       def iterator: Iterator[A]
 *    }}}
 *    They also need to provide a method `newBuilder`
 *    which creates a builder for collections of the same kind.
 *
 *    This trait implements `Iterable`'s `foreach`
 *    method by stepping through all elements using `iterator`.
 *    Subclasses should re-implement `foreach` with something more efficient,
 *    if possible.

 *    This trait adds methods `iterator`, `sameElements`,
 *    `takeRight`, `dropRight` to the methods inherited
 *    from trait <a href="../Iterable.html" target="ContentFrame">
 *    `Iterable`</a>.

 *    Note: This trait replaces every method that uses `break` in
 *    `IterableLike` by an iterator version.
 *
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8
 *  @tparam A    the element type of the collection
 *  @tparam Repr the type of the actual collection containing the elements.
 *
 *  @define Coll Iterable
 *  @define coll iterable collection
 */
trait IterableLike[+A, +Repr]
    extends Any
       with Equals
       with HasNewBuilder[A, Repr]
       with FilterMonadic[A, Repr]
       with IterableOnce[A] {
  self =>
  
  /********* BEGIN TRAVLIKE *************/
  
  import Iterable.breaks._

  /** The type implementing this iterable */
  protected type Self = Repr

  /** The collection of type $coll underlying this `IterableLike` object.
   *  By default this is implemented as the `IterableLike` object itself,
   *  but this can be overridden.
   */
  def repr: Repr = this.asInstanceOf[Repr]

  final def isIterableAgain: Boolean = true
  
  /** The underlying collection seen as an instance of `$Coll`.
   *  By default this is implemented as the current collection object itself,
   *  but this can be overridden.
   */
  protected[this] def thisCollection: Iterable[A] = this.asInstanceOf[Iterable[A]]

  /** A conversion from collections of type `Repr` to `$Coll` objects.
   *  By default this is implemented as just a cast, but this can be overridden.
   */
  protected[this] def toCollection(repr: Repr): Iterable[A] = repr.asInstanceOf[Iterable[A]]

  /** Creates a new builder for this collection type.
   */
  protected[this] def newBuilder: mutable.Builder[A, Repr]

  /** Applies a function `f` to all elements of this $coll.
   *
   *  @param  f   the function that is applied for its side-effect to every element.
   *              The result of function `f` is discarded.
   *
   *  @tparam  U  the type parameter describing the result of function `f`.
   *              This result will always be ignored. Typically `U` is `Unit`,
   *              but this is not necessary.
   *
   *  @usecase def foreach(f: A => Unit): Unit
   *    @inheritdoc
   *
   *    Note: this method underlies the implementation of most other bulk operations.
   *    It's important to implement this method in an efficient way.
   *
   */
  // def foreach[U](f: A => U): Unit

  /** Tests whether this $coll is empty.
   *
   *  @return    `true` if the $coll contain no elements, `false` otherwise.
   */
  // def isEmpty: Boolean = {
  //   var result = true
  //   breakable {
  //     for (x <- this) {
  //       result = false
  //       break
  //     }
  //   }
  //   result
  // }

  /** Tests whether this $coll is known to have a finite size.
   *  All strict collections are known to have finite size. For a non-strict
   *  collection such as `Stream`, the predicate returns `'''true'''` if all
   *  elements have been computed. It returns `'''false'''` if the stream is
   *  not yet evaluated to the end.
   *
   *  Note: many collection methods will not work on collections of infinite sizes.
   *
   *  @return  `'''true'''` if this collection is known to have finite size,
   *           `'''false'''` otherwise.
   */
  def hasDefiniteSize = true

  def ++[B >: A, That](that: IterableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    if (that.isInstanceOf[IndexedSeqLike[_, _]]) b.sizeHint(this, that.size)
    b ++= thisCollection
    b ++= that
    b.result
  }

  /** As with `++`, returns a new collection containing the elements from the left operand followed by the
   *  elements from the right operand.
   *
   *  It differs from `++` in that the right operand determines the type of
   *  the resulting collection rather than the left one.
   *  Mnemonic: the COLon is on the side of the new COLlection type.
   *
   *  @param that   the iterable to append.
   *  @tparam B     the element type of the returned collection.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` which contains all elements
   *                of this $coll followed by all elements of `that`.
   *
   *  @usecase def ++:[B](that: IterableOnce[B]): $Coll[B]
   *    @inheritdoc
   * 
   *    Example:
   *    {{{
   *      scala> val x = List(1)
   *      x: List[Int] = List(1)
   *
   *      scala> val y = LinkedList(2)
   *      y: scala.collection.mutable.LinkedList[Int] = LinkedList(2)
   *
   *      scala> val z = x ++: y
   *      z: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2)
   *    }}}
   *
   *    @return       a new $coll which contains all elements of this $coll
   *                  followed by all elements of `that`.
   */
  def ++:[B >: A, That](that: IterableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    if (that.isInstanceOf[IndexedSeqLike[_, _]]) b.sizeHint(this, that.size)
    b ++= that
    b ++= thisCollection
    b.result
  }

  /** As with `++`, returns a new collection containing the elements from the
   *  left operand followed by the elements from the right operand.
   *
   *  It differs from `++` in that the right operand determines the type of
   *  the resulting collection rather than the left one.
   *  Mnemonic: the COLon is on the side of the new COLlection type.
   *
   *  Example:
   *  {{{
   *     scala> val x = List(1)
   *     x: List[Int] = List(1)
   *
   *     scala> val y = LinkedList(2)
   *     y: scala.collection.mutable.LinkedList[Int] = LinkedList(2)
   *
   *     scala> val z = x ++: y
   *     z: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2)
   *  }}}
   *
   * This overload exists because: for the implementation of `++:` we should
   *  reuse that of `++` because many collections override it with more
   *  efficient versions.
   *
   *  Since `IterableOnce` has no `++` method, we have to implement that
   *  directly, but `Iterable` and down can use the overload.
   *
   *  @param that   the Iterable to append.
   *  @tparam B     the element type of the returned collection.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` which contains all elements
   *                of this $coll followed by all elements of `that`.
   */
  def ++:[B >: A, That](that: Iterable[B])(implicit bf: CanBuildFrom[Repr, B, That]): That =
    (that ++ this)(breakOut)

  def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    b.sizeHint(this)
    for (x <- this) b += f(x)
    b.result
  }

  def flatMap[B, That](f: A => IterableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    for (x <- this) b ++= f(x)
    b.result
  }

  /** Selects all elements of this $coll which satisfy a predicate.
   *
   *  @param p     the predicate used to test elements.
   *  @return      a new $coll consisting of all elements of this $coll that satisfy the given
   *               predicate `p`. The order of the elements is preserved.
   */
  def filter(p: A => Boolean): Repr = {
    val b = newBuilder
    for (x <- this)
      if (p(x)) b += x
    b.result
  }

  /** Selects all elements of this $coll which do not satisfy a predicate.
   *
   *  @param p     the predicate used to test elements.
   *  @return      a new $coll consisting of all elements of this $coll that do not satisfy the given
   *               predicate `p`. The order of the elements is preserved.
   */
  def filterNot(p: A => Boolean): Repr = filter(!p(_))

  def collect[B, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    for (x <- this) if (pf.isDefinedAt(x)) b += pf(x)
    b.result
  }

  /** Partitions this $coll in two ${coll}s according to a predicate.
   *
   *  @param p the predicate on which to partition.
   *  @return  a pair of ${coll}s: the first $coll consists of all elements that
   *           satisfy the predicate `p` and the second $coll consists of all elements
   *           that don't. The relative order of the elements in the resulting ${coll}s
   *           is the same as in the original $coll.
   */
  def partition(p: A => Boolean): (Repr, Repr) = {
    val l, r = newBuilder
    for (x <- this) (if (p(x)) l else r) += x
    (l.result, r.result)
  }

  def groupBy[K](f: A => K): immutable.Map[K, Repr] = {
    val m = mutable.Map.empty[K, Builder[A, Repr]]
    for (elem <- this) {
      val key = f(elem)
      val bldr = m.getOrElseUpdate(key, newBuilder)
      bldr += elem
    }
    val b = immutable.Map.newBuilder[K, Repr]
    for ((k, v) <- m)
      b += ((k, v.result))

    b.result
  }

  /** Tests whether a predicate holds for all elements of this $coll.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return        `true` if the given predicate `p` holds for all elements
   *                 of this $coll, otherwise `false`.
   */
  // def forall(p: A => Boolean): Boolean = {
  //   var result = true
  //   breakable {
  //     for (x <- this)
  //       if (!p(x)) { result = false; break }
  //   }
  //   result
  // }

  /** Tests whether a predicate holds for some of the elements of this $coll.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return        `true` if the given predicate `p` holds for some of the
   *                 elements of this $coll, otherwise `false`.
   */
  // def exists(p: A => Boolean): Boolean = {
  //   var result = false
  //   breakable {
  //     for (x <- this)
  //       if (p(x)) { result = true; break }
  //   }
  //   result
  // }

  /** Finds the first element of the $coll satisfying a predicate, if any.
   *
   *  $mayNotTerminateInf
   *  $orderDependent
   *
   *  @param p    the predicate used to test elements.
   *  @return     an option value containing the first element in the $coll
   *              that satisfies `p`, or `None` if none exists.
   */
  // def find(p: A => Boolean): Option[A] = {
  //   var result: Option[A] = None
  //   breakable {
  //     for (x <- this)
  //       if (p(x)) { result = Some(x); break }
  //   }
  //   result
  // }

  def scan[B >: A, That](z: B)(op: (B, B) => B)(implicit cbf: CanBuildFrom[Repr, B, That]): That = scanLeft(z)(op)

  def scanLeft[B, That](z: B)(op: (B, A) => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    b.sizeHint(this, 1)
    var acc = z
    b += acc
    for (x <- this) { acc = op(acc, x); b += acc }
    b.result
  }

  def scanRight[B, That](z: B)(op: (A, B) => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    var scanned = List(z)
    var acc = z
    for (x <- reversed) {
      acc = op(x, acc)
      scanned ::= acc
    }
    val b = bf(repr)
    for (elem <- scanned) b += elem
    b.result
  }

  /** Selects the first element of this $coll.
   *  $orderDependent
   *  @return  the first element of this $coll.
   *  @throws `NoSuchElementException` if the $coll is empty.
   */
  // def head: A = {
  //   var result: () => A = () => throw new NoSuchElementException
  //   breakable {
  //     for (x <- this) {
  //       result = () => x
  //       break
  //     }
  //   }
  //   result()
  // }

  /** Optionally selects the first element.
   *  $orderDependent
   *  @return  the first element of this $coll if it is nonempty,
   *           `None` if it is empty.
   */
  def headOption: Option[A] = if (isEmpty) None else Some(head)

  /** Selects all elements except the first.
   *  $orderDependent
   *  @return  a $coll consisting of all elements of this $coll
   *           except the first one.
   *  @throws `UnsupportedOperationException` if the $coll is empty.
   */
  def tail: Repr = {
    if (isEmpty) throw new UnsupportedOperationException("empty.tail")
    drop(1)
  }

  /** Selects the last element.
    * $orderDependent
    * @return The last element of this $coll.
    * @throws NoSuchElementException If the $coll is empty.
    */
  def last: A = {
    var lst = head
    for (x <- this)
      lst = x
    lst
  }

  /** Optionally selects the last element.
   *  $orderDependent
   *  @return  the last element of this $coll$ if it is nonempty,
   *           `None` if it is empty.
   */
  def lastOption: Option[A] = if (isEmpty) None else Some(last)

  /** Selects all elements except the last.
   *  $orderDependent
   *  @return  a $coll consisting of all elements of this $coll
   *           except the last one.
   *  @throws `UnsupportedOperationException` if the $coll is empty.
   */
  def init: Repr = {
    if (isEmpty) throw new UnsupportedOperationException("empty.init")
    var lst = head
    var follow = false
    val b = newBuilder
    b.sizeHint(this, -1)
    for (x <- this) {
      if (follow) b += lst
      else follow = true
      lst = x
    }
    b.result
  }
  // 
  // def take(n: Int): Repr = slice(0, n)
  // 
  // def drop(n: Int): Repr =
  //   if (n <= 0) {
  //     val b = newBuilder
  //     b.sizeHint(this)
  //     (b ++= thisCollection).result
  //   }
  //   else sliceWithKnownDelta(n, Int.MaxValue, -n)
  // 
  // def slice(from: Int, until: Int): Repr =
  //   sliceWithKnownBound(math.max(from, 0), until)

  // Precondition: from >= 0, until > 0, builder already configured for building.
  private[this] def sliceInternal(from: Int, until: Int, b: Builder[A, Repr]): Repr = {
    var i = 0
    breakable {
      for (x <- this) {
        if (i >= from) b += x
        i += 1
        if (i >= until) break
      }
    }
    b.result
  }
  // Precondition: from >= 0
  private[scala] def sliceWithKnownDelta(from: Int, until: Int, delta: Int): Repr = {
    val b = newBuilder
    if (until <= from) b.result
    else {
      b.sizeHint(this, delta)
      sliceInternal(from, until, b)
    }
  }
  // Precondition: from >= 0
  private[scala] def sliceWithKnownBound(from: Int, until: Int): Repr = {
    val b = newBuilder
    if (until <= from) b.result
    else {
      b.sizeHintBounded(until - from, this)
      sliceInternal(from, until, b)
    }
  }
  // 
  // def takeWhile(p: A => Boolean): Repr = {
  //   val b = newBuilder
  //   breakable {
  //     for (x <- this) {
  //       if (!p(x)) break
  //       b += x
  //     }
  //   }
  //   b.result
  // }

  def dropWhile(p: A => Boolean): Repr = {
    val b = newBuilder
    var go = false
    for (x <- this) {
      if (!go && !p(x)) go = true
      if (go) b += x
    }
    b.result
  }

  def span(p: A => Boolean): (Repr, Repr) = {
    val l, r = newBuilder
    var toLeft = true
    for (x <- this) {
      toLeft = toLeft && p(x)
      (if (toLeft) l else r) += x
    }
    (l.result, r.result)
  }

  def splitAt(n: Int): (Repr, Repr) = {
    val l, r = newBuilder
    l.sizeHintBounded(n, this)
    if (n >= 0) r.sizeHint(this, -n)
    var i = 0
    for (x <- this) {
      (if (i < n) l else r) += x
      i += 1
    }
    (l.result, r.result)
  }

  /** Iterates over the tails of this $coll. The first value will be this
   *  $coll and the final one will be an empty $coll, with the intervening
   *  values the results of successive applications of `tail`.
   *
   *  @return   an iterator over all the tails of this $coll
   *  @example  `List(1,2,3).tails = Iterator(List(1,2,3), List(2,3), List(3), Nil)`
   */
  def tails: Iterator[Repr] = iterateUntilEmpty(_.tail)

  /** Iterates over the inits of this $coll. The first value will be this
   *  $coll and the final one will be an empty $coll, with the intervening
   *  values the results of successive applications of `init`.
   *
   *  @return  an iterator over all the inits of this $coll
   *  @example  `List(1,2,3).inits = Iterator(List(1,2,3), List(1,2), List(1), Nil)`
   */
  def inits: Iterator[Repr] = iterateUntilEmpty(_.init)

  /** Copies elements of this $coll to an array.
   *  Fills the given array `xs` with at most `len` elements of
   *  this $coll, starting at position `start`.
   *  Copying will stop once either the end of the current $coll is reached,
   *  or the end of the array is reached, or `len` elements have been copied.
   *
   *  @param  xs     the array to fill.
   *  @param  start  the starting index.
   *  @param  len    the maximal number of elements to copy.
   *  @tparam B      the type of the elements of the array.
   *
   *
   *  @usecase def copyToArray(xs: Array[A], start: Int, len: Int): Unit
   *    @inheritdoc
   *
   *    $willNotTerminateInf
   */
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int) {
    var i = start
    val end = (start + len) min xs.length
    breakable {
      for (x <- this) {
        if (i >= end) break
        xs(i) = x
        i += 1
      }
    }
  }
  // override /*IterableLike*/ def copyToArray[B >: A](xs: Array[B], start: Int, len: Int) {
  //   var i = start
  //   val end = (start + len) min xs.length
  //   val it = iterator
  //   while (i < end && it.hasNext) {
  //     xs(i) = it.next
  //     i += 1
  //   }
  // }

  def toIterable: Iterable[A] = thisCollection
  def toIterator: Iterator[A] = iterator  // toSeq.iterator
  // 
  // override /*IterableLike*/ def toIterable: Iterable[A] =
  //   thisCollection
  // override /*IterableLike*/ def toIterator: Iterator[A] =
  //   iterator

  /** Converts this $coll to a string.
   *
   *  @return   a string representation of this collection. By default this
   *            string consists of the `stringPrefix` of this $coll, followed
   *            by all elements separated by commas and enclosed in parentheses.
   */
  override def toString = mkString(stringPrefix + "(", ", ", ")")

  /** Defines the prefix of this object's `toString` representation.
   *
   *  @return  a string representation which starts the result of `toString`
   *           applied to this $coll. By default the string prefix is the
   *           simple name of the collection class $coll.
   */
  def stringPrefix : String = {
    var string = repr.getClass.getName
    val idx1 = string.lastIndexOf('.' : Int)
    if (idx1 != -1) string = string.substring(idx1 + 1)
    val idx2 = string.indexOf('$')
    if (idx2 != -1) string = string.substring(0, idx2)
    string
  }

  /** Creates a non-strict filter of this $coll.
   *
   *  Note: the difference between `c filter p` and `c withFilter p` is that
   *        the former creates a new collection, whereas the latter only
   *        restricts the domain of subsequent `map`, `flatMap`, `foreach`,
   *        and `withFilter` operations.
   *  $orderDependent
   *
   *  @param p   the predicate used to test elements.
   *  @return    an object of class `WithFilter`, which supports
   *             `map`, `flatMap`, `foreach`, and `withFilter` operations.
   *             All these operations apply to those elements of this $coll
   *             which satisfy the predicate `p`.
   */
  def withFilter(p: A => Boolean): FilterMonadic[A, Repr] = new WithFilter(p)

  /** A class supporting filtered operations. Instances of this class are
   *  returned by method `withFilter`.
   */
  class WithFilter(p: A => Boolean) extends FilterMonadic[A, Repr] {

    /** Builds a new collection by applying a function to all elements of the
     *  outer $coll containing this `WithFilter` instance that satisfy predicate `p`.
     *
     *  @param f      the function to apply to each element.
     *  @tparam B     the element type of the returned collection.
     *  @tparam That  $thatinfo
     *  @param bf     $bfinfo
     *  @return       a new collection of type `That` resulting from applying
     *                the given function `f` to each element of the outer $coll
     *                that satisfies predicate `p` and collecting the results.
     *
     *  @usecase def map[B](f: A => B): $Coll[B]
     *    @inheritdoc
     *
     *    @return       a new $coll resulting from applying the given function
     *                  `f` to each element of the outer $coll that satisfies
     *                  predicate `p` and collecting the results.
     */
    def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
      val b = bf(repr)
      for (x <- self)
        if (p(x)) b += f(x)
      b.result
    }

    /** Builds a new collection by applying a function to all elements of the
     *  outer $coll containing this `WithFilter` instance that satisfy
     *  predicate `p` and concatenating the results.
     *
     *  @param f      the function to apply to each element.
     *  @tparam B     the element type of the returned collection.
     *  @tparam That  $thatinfo
     *  @param bf     $bfinfo
     *  @return       a new collection of type `That` resulting from applying
     *                the given collection-valued function `f` to each element
     *                of the outer $coll that satisfies predicate `p` and
     *                concatenating the results.
     *
     *  @usecase def flatMap[B](f: A => IterableOnce[B]): $Coll[B]
     *    @inheritdoc
     *
     *    The type of the resulting collection will be guided by the static type
     *    of the outer $coll.
     *
     *    @return       a new $coll resulting from applying the given
     *                  collection-valued function `f` to each element of the
     *                  outer $coll that satisfies predicate `p` and concatenating
     *                  the results.
     */
    def flatMap[B, That](f: A => IterableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
      val b = bf(repr)
      for (x <- self)
        if (p(x)) b ++= f(x)
      b.result
    }

    /** Applies a function `f` to all elements of the outer $coll containing
     *  this `WithFilter` instance that satisfy predicate `p`.
     *
     *  @param  f   the function that is applied for its side-effect to every element.
     *              The result of function `f` is discarded.
     *
     *  @tparam  U  the type parameter describing the result of function `f`.
     *              This result will always be ignored. Typically `U` is `Unit`,
     *              but this is not necessary.
     *
     *  @usecase def foreach(f: A => Unit): Unit
     *    @inheritdoc
     */
    def foreach[U](f: A => U): Unit =
      for (x <- self)
        if (p(x)) f(x)

    /** Further refines the filter for this $coll.
     *
     *  @param q   the predicate used to test elements.
     *  @return    an object of class `WithFilter`, which supports
     *             `map`, `flatMap`, `foreach`, and `withFilter` operations.
     *             All these operations apply to those elements of this $coll which
     *             satisfy the predicate `q` in addition to the predicate `p`.
     */
    def withFilter(q: A => Boolean): WithFilter =
      new WithFilter(x => p(x) && q(x))
  }

  // A helper for tails and inits.
  private def iterateUntilEmpty(f: Iterable[A @uV] => Iterable[A @uV]): Iterator[Repr] = {
    val it = Iterator.iterate(thisCollection)(f) takeWhile (x => !x.isEmpty)
    it ++ Iterator(Nil) map (x => (newBuilder ++= x).result)
  }

  /************************ END TRAVLIKE ***************************/

  // def iterator: Iterator[A]
  //
  // /** Checks if the other iterable collection contains the same elements in the same order as this $coll.
  //  *
  //  *  @param that  the collection to compare with.
  //  *  @tparam A1   the type of the elements of collection `that`.
  //  *  @return `true`, if both collections contain the same elements in the same order, `false` otherwise.
  //  *
  //  *  @usecase  def sameElements(that: Iterable[A]): Boolean
  //  *    @inheritdoc
  //  *
  //  *    $orderDependent
  //  *    $willNotTerminateInf
  //  *
  //  *    @param that  the collection to compare with.
  //  *    @return `true`, if both collections contain the same elements in the same order, `false` otherwise.
  //  */
  // def sameElements[A1 >: A](that: Iterable[A1]): Boolean
  //
  // /** Returns a $coll formed from this $coll and another iterable collection
  //  *  by combining corresponding elements in pairs.
  //  *  If one of the two collections is longer than the other, its remaining elements are ignored.
  //  *
  //  *  @param   that  The iterable providing the second half of each result pair
  //  *  @tparam  A1    the type of the first half of the returned pairs (this is always a supertype
  //  *                 of the collection's element type `A`).
  //  *  @tparam  B     the type of the second half of the returned pairs
  //  *  @tparam  That  $zipthatinfo
  //  *  @param   bf    $zipbfinfo
  //  *  @return        a new collection of type `That` containing pairs consisting of
  //  *                 corresponding elements of this $coll and `that`. The length
  //  *                 of the returned collection is the minimum of the lengths of this $coll and `that`.
  //  *
  //  *  @usecase def zip[B](that: Iterable[B]): $Coll[(A, B)]
  //  *    @inheritdoc
  //  *
  //  *    $orderDependent
  //  *
  //  *    @param   that  The iterable providing the second half of each result pair
  //  *    @tparam  B     the type of the second half of the returned pairs
  //  *    @return        a new $coll containing pairs consisting of
  //  *                   corresponding elements of this $coll and `that`. The length
  //  *                   of the returned collection is the minimum of the lengths of this $coll and `that`.
  //  */
  // def zip[A1 >: A, B, That](that: Iterable[B])(implicit bf: CBF[Repr, (A1, B), That]): That
  //
  // /** Zips this $coll with its indices.
  //  *
  //  *  @tparam  A1    the type of the first half of the returned pairs (this is always a supertype
  //  *                 of the collection's element type `A`).
  //  *  @tparam  That  the class of the returned collection. Where possible, `That` is
  //  *                 the same class as the current collection class `Repr`, but this
  //  *                 depends on the element type `(A1, Int)` being admissible for that class,
  //  *                 which means that an implicit instance of type `CanBuildFrom[Repr, (A1, Int), That]`.
  //  *                 is found.
  //  *  @param  bf     an implicit value of class `CanBuildFrom` which determines the
  //  *                 result class `That` from the current representation type `Repr`
  //  *                 and the new element type `(A1, Int)`.
  //  *  @return        A new collection of type `That` containing pairs consisting of all elements of this
  //  *                 $coll paired with their index. Indices start at `0`.
  //  *
  //  *  @usecase def zipWithIndex: $Coll[(A, Int)]
  //  *    @inheritdoc
  //  *
  //  *    $orderDependent
  //  *
  //  *    @return        A new $coll containing pairs consisting of all elements of this
  //  *                   $coll paired with their index. Indices start at `0`.
  //  *    @example
  //  *      `List("a", "b", "c").zipWithIndex = List(("a", 0), ("b", 1), ("c", 2))`
  //  *
  //  */
  // def zipWithIndex[A1 >: A, That](implicit bf: CBF[Repr, (A1, Int), That]): That
  //
  // /** Returns a $coll formed from this $coll and another iterable collection
  //  *  by combining corresponding elements in pairs.
  //  *  If one of the two collections is shorter than the other,
  //  *  placeholder elements are used to extend the shorter collection to the length of the longer.
  //  *
  //  *  @param that     the iterable providing the second half of each result pair
  //  *  @param thisElem the element to be used to fill up the result if this $coll is shorter than `that`.
  //  *  @param thatElem the element to be used to fill up the result if `that` is shorter than this $coll.
  //  *  @return        a new collection of type `That` containing pairs consisting of
  //  *                 corresponding elements of this $coll and `that`. The length
  //  *                 of the returned collection is the maximum of the lengths of this $coll and `that`.
  //  *                 If this $coll is shorter than `that`, `thisElem` values are used to pad the result.
  //  *                 If `that` is shorter than this $coll, `thatElem` values are used to pad the result.
  //  *
  //  *  @usecase def zipAll[B](that: Iterable[B], thisElem: A, thatElem: B): $Coll[(A, B)]
  //  *    @inheritdoc
  //  *
  //  *    $orderDependent
  //  *
  //  *    @param   that  The iterable providing the second half of each result pair
  //  *    @param thisElem the element to be used to fill up the result if this $coll is shorter than `that`.
  //  *    @param thatElem the element to be used to fill up the result if `that` is shorter than this $coll.
  //  *    @tparam  B     the type of the second half of the returned pairs
  //  *    @return        a new $coll containing pairs consisting of
  //  *                   corresponding elements of this $coll and `that`. The length
  //  *                   of the returned collection is the maximum of the lengths of this $coll and `that`.
  //  *                   If this $coll is shorter than `that`, `thisElem` values are used to pad the result.
  //  *                   If `that` is shorter than this $coll, `thatElem` values are used to pad the result.
  //  */
  // def zipAll[B, A1 >: A, That](that: Iterable[B], thisElem: A1, thatElem: B)(implicit bf: CBF[Repr, (A1, B), That]): That
  // 
  // override protected[this] def thisCollection: Iterable[A] = this.asInstanceOf[Iterable[A]]
  // override protected[this] def toCollection(repr: Repr): Iterable[A] = repr.asInstanceOf[Iterable[A]]

  /** Creates a new iterator over all elements contained in this iterable object.
   *
   *  @return the new iterator
   */
  def iterator: Iterator[A]
  
  // = toBuffer.iterator

  /** Applies a function `f` to all elements of this $coll.
   *
   *    Note: this method underlies the implementation of most other bulk operations.
   *    Subclasses should re-implement this method if a more efficient implementation exists.
   *
   *  @usecase def foreach(f: A => Unit): Unit
   *    @inheritdoc
   */
  def foreach[U](f: A => U): Unit = iterator foreach f
  // def foreach[U](f: A => U): Unit
  
  def forall(p: A => Boolean): Boolean = iterator forall p
  def exists(p: A => Boolean): Boolean = iterator exists p
  def find(p: A => Boolean): Option[A] = iterator find p
  def isEmpty = !iterator.hasNext
  override def foldRight[B](z: B)(op: (A, B) => B): B = iterator.foldRight(z)(op)
  override def reduceRight[B >: A](op: (A, B) => B): B = iterator.reduceRight(op)
  def head: A = iterator.next

  // override /*IterableLike*/ def forall(p: A => Boolean): Boolean =
  //   iterator.forall(p)
  // override /*IterableLike*/ def exists(p: A => Boolean): Boolean =
  //   iterator.exists(p)
  // override /*IterableLike*/ def find(p: A => Boolean): Option[A] =
  //   iterator.find(p)
  // override /*IterableLike*/ def isEmpty: Boolean =
  //   !iterator.hasNext
  // override /*IterableLike*/ def foldRight[B](z: B)(op: (A, B) => B): B =
  //   iterator.foldRight(z)(op)
  // override /*IterableLike*/ def reduceRight[B >: A](op: (A, B) => B): B =
  //   iterator.reduceRight(op)
  // override /*IterableLike*/ def head: A =
  //   iterator.next

  // override 
  /*IterableLike*/ def slice(from: Int, until: Int): Repr = {
    val lo = math.max(from, 0)
    val elems = until - lo
    val b = newBuilder
    if (elems <= 0) b.result
    else {
      b.sizeHintBounded(elems, this)
      var i = 0
      val it = iterator drop lo
      while (i < elems && it.hasNext) {
        b += it.next
        i += 1
      }
      b.result
    }
  }

  // override 
  /*IterableLike*/ def take(n: Int): Repr = {
    val b = newBuilder

    if (n <= 0) b.result
    else {
      b.sizeHintBounded(n, this)
      var i = 0
      val it = iterator
      while (i < n && it.hasNext) {
        b += it.next
        i += 1
      }
      b.result
    }
  }

  // override 
  /*IterableLike*/ def drop(n: Int): Repr = {
    val b = newBuilder
    val lo = math.max(0, n)
    b.sizeHint(this, -lo)
    var i = 0
    val it = iterator
    while (i < n && it.hasNext) {
      it.next
      i += 1
    }
    (b ++= it).result
  }

  // override 
  /*IterableLike*/ def takeWhile(p: A => Boolean): Repr = {
    val b = newBuilder
    val it = iterator
    while (it.hasNext) {
      val x = it.next
      if (!p(x)) return b.result
      b += x
    }
    b.result
  }

  /** Partitions elements in fixed size ${coll}s.
   *  @see [[scala.collection.Iterator]], method `grouped`
   *
   *  @param size the number of elements per group
   *  @return An iterator producing ${coll}s of size `size`, except the
   *          last will be truncated if the elements don't divide evenly.
   */
  def grouped(size: Int): Iterator[Repr] =
    for (xs <- iterator grouped size) yield {
      val b = newBuilder
      b ++= xs
      b.result
    }

  /** Groups elements in fixed size blocks by passing a "sliding window"
   *  over them (as opposed to partitioning them, as is done in grouped.)
   *  @see [[scala.collection.Iterator]], method `sliding`
   *
   *  @param size the number of elements per group
   *  @return An iterator producing ${coll}s of size `size`, except the
   *          last and the only element will be truncated if there are
   *          fewer elements than size.
   */
  def sliding(size: Int): Iterator[Repr] = sliding(size, 1)
  
  /** Groups elements in fixed size blocks by passing a "sliding window"
   *  over them (as opposed to partitioning them, as is done in grouped.)
   *  @see [[scala.collection.Iterator]], method `sliding`
   *
   *  @param size the number of elements per group
   *  @param step the distance between the first elements of successive
   *         groups (defaults to 1)
   *  @return An iterator producing ${coll}s of size `size`, except the
   *          last and the only element will be truncated if there are
   *          fewer elements than size.
   */
  def sliding(size: Int, step: Int): Iterator[Repr] =
    for (xs <- iterator.sliding(size, step)) yield {
      val b = newBuilder
      b ++= xs
      b.result
    }

  /** Selects last ''n'' elements.
   *  $orderDependent
   *
   *  @param n the number of elements to take
   *  @return a $coll consisting only of the last `n` elements of this $coll, or else the
   *          whole $coll, if it has less than `n` elements.
   */
  def takeRight(n: Int): Repr = {
    val b = newBuilder
    b.sizeHintBounded(n, this)
    val lead = this.iterator drop n
    var go = false
    for (x <- this) {
      if (lead.hasNext) lead.next
      else go = true
      if (go) b += x
    }
    b.result
  }

  /** Selects all elements except last ''n'' ones.
   *  $orderDependent
   *
   *  @param  n    The number of elements to take
   *  @return a $coll consisting of all elements of this $coll except the last `n` ones, or else the
   *          empty $coll, if this $coll has less than `n` elements.
   */
  def dropRight(n: Int): Repr = {
    val b = newBuilder
    if (n >= 0) b.sizeHint(this, -n)
    val lead = iterator drop n
    val it = iterator
    while (lead.hasNext) {
      b += it.next
      lead.next
    }
    b.result
  }

  def zip[A1 >: A, B, That](that: Iterable[B])(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
    val b = bf(repr)
    val these = this.iterator
    val those = that.iterator
    while (these.hasNext && those.hasNext)
      b += ((these.next, those.next))
    b.result
  }

  def zipAll[B, A1 >: A, That](that: Iterable[B], thisElem: A1, thatElem: B)(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
    val b = bf(repr)
    val these = this.iterator
    val those = that.iterator
    while (these.hasNext && those.hasNext)
      b += ((these.next, those.next))
    while (these.hasNext)
      b += ((these.next, thatElem))
    while (those.hasNext)
      b += ((thisElem, those.next))
    b.result
  }

  def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[Repr, (A1, Int), That]): That = {
    val b = bf(repr)
    var i = 0
    for (x <- this) {
      b += ((x, i))
      i +=1
    }
    b.result
  }

  def sameElements[B >: A](that: Iterable[B]): Boolean = {
    val these = this.iterator
    val those = that.iterator
    while (these.hasNext && those.hasNext)
      if (these.next != those.next)
        return false

    !these.hasNext && !those.hasNext
  }

  /** Method called from equality methods, so that user-defined subclasses can
   *  refuse to be equal to other collections of the same kind.
   *  @param   that   The object with which this $coll should be compared
   *  @return  `true`, if this $coll can possibly equal `that`, `false` otherwise. The test
   *           takes into consideration only the run-time types of objects but ignores their elements.
   */
  override /*IterableLike*/ def canEqual(that: Any) = true
}
