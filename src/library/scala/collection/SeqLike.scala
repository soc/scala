/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

import mutable.{ ListBuffer, ArraySeq }
import immutable.{ List, Range }
import generic._
import scala.math.Ordering

/** A template trait for sequences of type `Seq[A]`
 *  $seqInfo
 *
 *  @define seqInfo
 *  Sequences are special cases of iterable collections of class `Iterable`.
 *  Unlike iterables, sequences always have a defined order of elements.
 *  Sequences provide a method `apply` for indexing. Indices range from `0` up to the `length` of
 *  a sequence. Sequences support a number to find occurrences of elements or subsequences, including
 *  `segmentLength`, `prefixLength`, `indexWhere`, `indexOf`, `lastIndexWhere`, `lastIndexOf`,
 *  `startsWith`, `endsWith`, `indexOfSlice`.
 *
 *  Another way to see a sequence is as a `PartialFunction` from `Int` values
 *  to the element type of the sequence. The `isDefinedAt` method of a sequence
 *  returns `true` for the interval from `0` until `length`.
 *
 *  Sequences can be accessed in reverse order of their elements, using methods
 *  `reverse` and `reverseIterator`.
 *
 *  Sequences have two principal subtraits, `IndexedSeq` and `LinearSeq`, which give different guarantees for performance.
 *  An `IndexedSeq` provides fast random-access of elements and a fast `length` operation.
 *  A `LinearSeq` provides fast access only to the first element via `head`, but also
 *  has a fast `tail` operation.
 *
 *  @tparam A    the element type of the collection
 *  @tparam Repr the type of the actual collection containing the elements.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.0, 16/07/2003
 *  @since   2.8
 *
 *  @define Coll `Seq`
 *  @define coll sequence
 *  @define thatinfo the class of the returned collection. Where possible, `That` is
 *    the same class as the current collection class `Repr`, but this
 *    depends on the element type `B` being admissible for that class,
 *    which means that an implicit instance of type `CanBuildFrom[Repr, B, That]`
 *    is found.
 *  @define bfinfo an implicit value of class `CanBuildFrom` which determines the
 *    result class `That` from the current representation type `Repr`
 *    and the new element type `B`.
 *  @define orderDependent
 *  @define orderDependentFold
 */
trait SeqLike[+A, +Repr] extends Any with IterableLike[A, Repr] {
  self =>
  //
  // /** Selects an element by its index in the $coll.
  //  *
  //  * Example:
  //  *
  //  * {{{
  //  *    scala> val x = LinkedList(1, 2, 3, 4, 5)
  //  *    x: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2, 3, 4, 5)
  //  *
  //  *    scala> x(3)
  //  *    res1: Int = 4
  //  * }}}
  //  *
  //  *  @param  idx  The index to select.
  //  *  @return the element of this $coll at index `idx`, where `0` indicates the first element.
  //  *  @throws `IndexOutOfBoundsException` if `idx` does not satisfy `0 <= idx < length`.
  //  */
  // def apply(idx: Int): A
  //
  // /** The length of the $coll.
  //  *
  //  *  $willNotTerminateInf
  //  *
  //  *  Note: `xs.length` and `xs.size` yield the same result.
  //  *
  //  *  @return     the number of elements in this $coll.
  //  */
  // def length: Int

  /** Tests whether this $coll contains given index.
   *
   *  The implementations of methods `apply` and `isDefinedAt` turn a `Seq[A]` into
   *  a `PartialFunction[Int, A]`.
   *
   * @param    idx     the index to test
   * @return   `true` if this $coll contains an element at position `idx`, `false` otherwise.
   */
  def isDefinedAt(idx: Int): Boolean = (idx >= 0) && (idx < length)
  //
  // /** Computes length of longest segment whose elements all satisfy some predicate.
  //  *
  //  *  $mayNotTerminateInf
  //  *
  //  *  @param   p     the predicate used to test elements.
  //  *  @param   from  the index where the search starts.
  //  *  @return  the length of the longest segment of this $coll starting from index `from`
  //  *           such that every element of the segment satisfies the predicate `p`.
  //  */
  // def segmentLength(p: A => Boolean, from: Int): Int

  /** Returns the length of the longest prefix whose elements all satisfy some predicate.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return  the length of the longest prefix of this $coll
   *           such that every element of the segment satisfies the predicate `p`.
   */
  def prefixLength(p: A => Boolean): Int = segmentLength(p, 0)

  //
  // /** Finds index of the first element satisfying some predicate after or at some start index.
  //  *
  //  *  $mayNotTerminateInf
  //  *
  //  *  @param   p     the predicate used to test elements.
  //  *  @param   from   the start index
  //  *  @return  the index `>= from` of the first element of this $coll that satisfies the predicate `p`,
  //  *           or `-1`, if none exists.
  //  */
  // def indexWhere(p: A => Boolean, from: Int): Int

  /** Finds index of first element satisfying some predicate.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return  the index of the first element of this $coll that satisfies the predicate `p`,
   *           or `-1`, if none exists.
   */
  def indexWhere(p: A => Boolean): Int = indexWhere(p, 0)

  /** Finds index of first occurrence of some value in this $coll.
   *
   *  @param   elem   the element value to search for.
   *  @tparam  B      the type of the element `elem`.
   *  @return  the index of the first element of this $coll that is equal (wrt `==`)
   *           to `elem`, or `-1`, if none exists.
   *
   *  @usecase def indexOf(elem: A): Int
   *    @inheritdoc
   *
   *    $mayNotTerminateInf
   *
   */
  def indexOf[B >: A](elem: B): Int = indexOf(elem, 0)

  /** Finds index of first occurrence of some value in this $coll after or at some start index.
   *
   *  @param   elem   the element value to search for.
   *  @tparam  B      the type of the element `elem`.
   *  @param   from   the start index
   *  @return  the index `>= from` of the first element of this $coll that is equal (wrt `==`)
   *           to `elem`, or `-1`, if none exists.
   *
   *  @usecase def indexOf(elem: A, from: Int): Int
   *    @inheritdoc
   *
   *    $mayNotTerminateInf
   *
   */
  def indexOf[B >: A](elem: B, from: Int): Int = indexWhere(elem == _, from)

  /** Finds index of last occurrence of some value in this $coll.
   *
   *  @param   elem   the element value to search for.
   *  @tparam  B      the type of the element `elem`.
   *  @return  the index of the last element of this $coll that is equal (wrt `==`)
   *           to `elem`, or `-1`, if none exists.
   *
   *  @usecase def lastIndexOf(elem: A): Int
   *    @inheritdoc
   *
   *    $willNotTerminateInf
   *
   */
  def lastIndexOf[B >: A](elem: B): Int = lastIndexWhere(elem == _)

  /** Finds index of last occurrence of some value in this $coll before or at a given end index.
   *
   *  @param   elem   the element value to search for.
   *  @param   end    the end index.
   *  @tparam  B      the type of the element `elem`.
   *  @return  the index `<= end` of the last element of this $coll that is equal (wrt `==`)
   *           to `elem`, or `-1`, if none exists.
   *
   *  @usecase def lastIndexOf(elem: A, end: Int): Int
   *    @inheritdoc
   */
  def lastIndexOf[B >: A](elem: B, end: Int): Int = lastIndexWhere(elem == _, end)

  /** Finds index of last element satisfying some predicate.
   *
   *  $willNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return  the index of the last element of this $coll that satisfies the predicate `p`,
   *           or `-1`, if none exists.
   */
  def lastIndexWhere(p: A => Boolean): Int = lastIndexWhere(p, length - 1)
  //
  // /** Finds index of last element satisfying some predicate before or at given end index.
  //  *
  //  *  @param   p     the predicate used to test elements.
  //  *  @return  the index `<= end` of the last element of this $coll that satisfies the predicate `p`,
  //  *           or `-1`, if none exists.
  //  */
  // def lastIndexWhere(p: A => Boolean, end: Int): Int
  //
  // /** Returns new $coll wih elements in reversed order.
  //  *
  //  *  $willNotTerminateInf
  //  *
  //  *  @return A new $coll with all elements of this $coll in reversed order.
  //  */
  // def reverse: Repr
  //
  // /**
  //  *  Builds a new collection by applying a function to all elements of this $coll and
  //  *  collecting the results in reversed order.
  //  *
  //  *  @param f      the function to apply to each element.
  //  *  @tparam B     the element type of the returned collection.
  //  *  @tparam That  $thatinfo
  //  *  @param bf     $bfinfo
  //  *  @return       a new collection of type `That` resulting from applying the given function
  //  *                `f` to each element of this $coll and collecting the results in reversed order.
  //  *
  //  *  @usecase def reverseMap[B](f: A => B): $Coll[B]
  //  *    @inheritdoc
  //  *
  //  *    $willNotTerminateInf
  //  *
  //  *    Note: `xs.reverseMap(f)` is the same as `xs.reverse.map(f)` but might be more efficient.
  //  *
  //  *    @return       a new $coll resulting from applying the given function
  //  *                  `f` to each element of this $coll and collecting the results in reversed order.
  //  */
  // def reverseMap[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, B, That]): That

  /** Tests whether this $coll starts with the given sequence.
   *
   * @param  that    the sequence to test
   * @return `true` if this collection has `that` as a prefix, `false` otherwise.
   */
  def startsWith[B](that: Seq[B]): Boolean = startsWith(that, 0)

  // /** Tests whether this $coll contains the given sequence at a given index.
  //  *
  //  * '''Note''': If the both the receiver object `this` and the argument
  //  * `that` are infinite sequences this method may not terminate.
  //  *
  //  * @param  that    the sequence to test
  //  * @param  offset  the index where the sequence is searched.
  //  * @return `true` if the sequence `that` is contained in this $coll at
  //  *         index `offset`, otherwise `false`.
  //  */
  // def startsWith[B](that: Seq[B], offset: Int): Boolean
  //
  // /** Tests whether this $coll ends with the given sequence.
  //  *  $willNotTerminateInf
  //  *  @param  that    the sequence to test
  //  *  @return `true` if this $coll has `that` as a suffix, `false` otherwise.
  //  */
  // def endsWith[B](that: Seq[B]): Boolean
  //
  // /** Produces a new $coll where a slice of elements in this $coll is replaced by another sequence.
  //  *
  //  *  @param  from     the index of the first replaced element
  //  *  @param  patch    the replacement sequence
  //  *  @param  replaced the number of elements to drop in the original $coll
  //  *  @tparam B        the element type of the returned $coll.
  //  *  @tparam That     $thatinfo
  //  *  @param bf        $bfinfo
  //  *  @return          a new $coll consisting of all elements of this $coll
  //  *                   except that `replaced` elements starting from `from` are replaced
  //  *                   by `patch`.
  //  *
  //  *  @usecase def patch(from: Int, that: Seq[A], replaced: Int): $Coll[A]
  //  *    @inheritdoc
  //  *
  //  *    @return          a new $coll consisting of all elements of this $coll
  //  *                     except that `replaced` elements starting from `from` are replaced
  //  *                     by `patch`.
  //  */
  // def patch[B >: A, That](from: Int, patch: Seq[B], replaced: Int)(implicit bf: CanBuildFrom[Repr, B, That]): That
  //
  // /** A copy of this $coll with one single replaced element.
  //  *  @param  index  the position of the replacement
  //  *  @param  elem   the replacing element
  //  *  @tparam B        the element type of the returned $coll.
  //  *  @tparam That     $thatinfo
  //  *  @param bf        $bfinfo
  //  *  @return a new $coll` which is a copy of this $coll with the element at position `index` replaced by `elem`.
  //  *
  //  *  @usecase def updated(index: Int, elem: A): $Coll[A]
  //  *    @inheritdoc
  //  *
  //  *    @return a copy of this $coll with the element at position `index` replaced by `elem`.
  //  */
  // def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That
  //
  // /** A copy of the $coll with an element prepended.
  //  *
  //  *  @param  elem   the prepended element
  //  *  @tparam B      the element type of the returned $coll.
  //  *  @tparam That   $thatinfo
  //  *  @param bf      $bfinfo
  //  *  @return a new collection of type `That` consisting of `elem` followed
  //  *          by all elements of this $coll.
  //  *
  //  *  @usecase def +:(elem: A): $Coll[A]
  //  *    @inheritdoc
  //  *
  //  *    Note that :-ending operators are right associative (see example).
  //  *    A mnemonic for `+:` vs. `:+` is: the COLon goes on the COLlection side.
  //  *
  //  *    Also, the original $coll is not modified, so you will want to capture the result.
  //  *
  //  *    Example:
  //  *    {{{
  //  *      scala> val x = LinkedList(1)
  //  *      x: scala.collection.mutable.LinkedList[Int] = LinkedList(1)
  //  *
  //  *      scala> val y = 2 +: x
  //  *      y: scala.collection.mutable.LinkedList[Int] = LinkedList(2, 1)
  //  *
  //  *      scala> println(x)
  //  *      LinkedList(1)
  //  *    }}}
  //  *
  //  *    @return a new $coll consisting of `elem` followed
  //  *            by all elements of this $coll.
  //  */
  // def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That
  //
  // /** A copy of this $coll with an element appended.
  //  *
  //  *  A mnemonic for `+:` vs. `:+` is: the COLon goes on the COLlection side.
  //  *
  //  *  @param  elem   the appended element
  //  *  @tparam B      the element type of the returned $coll.
  //  *  @tparam That   $thatinfo
  //  *  @param bf      $bfinfo
  //  *  @return a new collection of type `That` consisting of
  //  *          all elements of this $coll followed by `elem`.
  //  *
  //  *  @usecase def :+(elem: A): $Coll[A]
  //  *    @inheritdoc
  //  *
  //  *    $willNotTerminateInf
  //  *
  //  *    Example:
  //  *    {{{
  //  *       scala> import scala.collection.mutable.LinkedList
  //  *       import scala.collection.mutable.LinkedList
  //  *
  //  *       scala> val a = LinkedList(1)
  //  *       a: scala.collection.mutable.LinkedList[Int] = LinkedList(1)
  //  *
  //  *       scala> val b = a :+ 2
  //  *       b: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2)
  //  *
  //  *       scala> println(a)
  //  *       LinkedList(1)
  //  *    }}}
  //  *
  //  *    @return a new $coll consisting of
  //  *            all elements of this $coll followed by `elem`.
  //  */
  // def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That
  //
  // /** Tests whether every element of this $coll relates to the
  //  *  corresponding element of another sequence by satisfying a test predicate.
  //  *
  //  *  @param   that  the other sequence
  //  *  @param   p     the test predicate, which relates elements from both sequences
  //  *  @tparam  B     the type of the elements of `that`
  //  *  @return  `true` if both sequences have the same length and
  //  *                  `p(x, y)` is `true` for all corresponding elements `x` of this $coll
  //  *                  and `y` of `that`, otherwise `false`.
  //  */
  // def corresponds[B](that: Seq[B])(p: (A, B) => Boolean): Boolean
  //
  // def toSeq: Seq[A]

  /** Produces a new sequence which contains all elements of this $coll and also all elements of
   *  a given sequence. `xs union ys`  is equivalent to `xs ++ ys`.
   *
   *  @param that   the sequence to add.
   *  @tparam B     the element type of the returned $coll.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` which contains all elements of this $coll
   *                followed by all elements of `that`.
   *
   *  @usecase def union(that: Seq[A]): $Coll[A]
   *    @inheritdoc
   *
   *    Another way to express this
   *    is that `xs union ys` computes the order-presevring multi-set union of `xs` and `ys`.
   *    `union` is hence a counter-part of `diff` and `intersect` which also work on multi-sets.
   *
   *    $willNotTerminateInf
   *
   *    @return       a new $coll which contains all elements of this $coll
   *                  followed by all elements of `that`.
   */
  def union[B >: A, That](that: Seq[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = this ++ that
  //
  // /** Computes the multiset difference between this $coll and another sequence.
  //  *
  //  *  @param that   the sequence of elements to remove
  //  *  @tparam B     the element type of the returned $coll.
  //  *  @return       a new collection of type `That` which contains all elements of this $coll
  //  *                except some of occurrences of elements that also appear in `that`.
  //  *                If an element value `x` appears
  //  *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will not form
  //  *                part of the result, but any following occurrences will.
  //  *
  //  *  @usecase def diff(that: Seq[A]): $Coll[A]
  //  *    @inheritdoc
  //  *
  //  *    $willNotTerminateInf
  //  *
  //  *    @return       a new $coll which contains all elements of this $coll
  //  *                  except some of occurrences of elements that also appear in `that`.
  //  *                  If an element value `x` appears
  //  *                  ''n'' times in `that`, then the first ''n'' occurrences of `x` will not form
  //  *                  part of the result, but any following occurrences will.
  //  */
  // def diff[B >: A](that: Seq[B]): Repr
  //
  // /** Computes the multiset intersection between this $coll and another sequence.
  //  *
  //  *  @param that   the sequence of elements to intersect with.
  //  *  @tparam B     the element type of the returned $coll.
  //  *  @return       a new collection of type `That` which contains all elements of this $coll
  //  *                which also appear in `that`.
  //  *                If an element value `x` appears
  //  *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will be retained
  //  *                in the result, but any following occurrences will be omitted.
  //  *
  //  *  @usecase def intersect(that: Seq[A]): $Coll[A]
  //  *    @inheritdoc
  //  *
  //  *    $mayNotTerminateInf
  //  *
  //  *    @return       a new $coll which contains all elements of this $coll
  //  *                  which also appear in `that`.
  //  *                  If an element value `x` appears
  //  *                  ''n'' times in `that`, then the first ''n'' occurrences of `x` will be retained
  //  *                  in the result, but any following occurrences will be omitted.
  //  */
  // def intersect[B >: A](that: Seq[B]): Repr
  //
  // /** Builds a new $coll from this $coll without any duplicate elements.
  //  *  $willNotTerminateInf
  //  *
  //  *  @return  A new $coll which contains the first occurrence of every element of this $coll.
  //  */
  // def distinct: Repr

  /** Hashcodes for $Coll produce a value from the hashcodes of all the
   *  elements of the $coll.
   */
  override def hashCode() = scala.util.hashing.MurmurHash3.seqHash(this.toSeq)

  /** The equals method for arbitrary sequences. Compares this sequence to
   *  some other object.
   *  @param    that  The object to compare the sequence to
   *  @return   `true` if `that` is a sequence that has the same elements as
   *            this sequence in the same order, `false` otherwise
   */
  override def equals(that: Any): Boolean = that match {
    case that: Seq[_] => (that canEqual this) && (this corresponds that)(_ == _)
    case _            => false
  }

  override protected[this] def thisCollection: Seq[A] = this.asInstanceOf[Seq[A]]
  override protected[this] def toCollection(repr: Repr): Seq[A] = repr.asInstanceOf[Seq[A]]

  def length: Int

  def apply(idx: Int): A

  /** Compares the length of this $coll to a test value.
   *
   *   @param   len   the test value that gets compared with the length.
   *   @return  A value `x` where
   *   {{{
   *        x <  0       if this.length <  len
   *        x == 0       if this.length == len
   *        x >  0       if this.length >  len
   *   }}}
   *  The method as implemented here does not call `length` directly; its running time
   *  is `O(length min len)` instead of `O(length)`. The method should be overwritten
   *  if computing `length` is cheap.
   */
  def lengthCompare(len: Int): Int = {
    var i = 0
    val it = iterator
    while (it.hasNext && i <= len) {
      it.next()
      i += 1
    }
    i - len
  }

  /** The size of this $coll, equivalent to `length`.
   *
   *  $willNotTerminateInf
   */
  override def size = length

  def segmentLength(p: A => Boolean, from: Int): Int = {
    var i = 0
    var it = iterator.drop(from)
    while (it.hasNext && p(it.next()))
      i += 1
    i
  }

  def indexWhere(p: A => Boolean, from: Int): Int = {
    var i = from
    var it = iterator.drop(from)
    while (it.hasNext) {
      if (p(it.next())) return i
      else i += 1
    }

    -1
  }

  def lastIndexWhere(p: A => Boolean, end: Int): Int = {
    var i = length - 1
    val it = reverseIterator
    while (it.hasNext && { val elem = it.next; (i > end || !p(elem)) }) i -= 1
    i
  }

  def reverse: Repr = {
    var xs: List[A] = Nil
    for (x <- this)
      xs = x :: xs
    val b = newBuilder
    b.sizeHint(this)
    for (x <- xs)
      b += x
    b.result
  }

  def reverseMap[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    var xs: List[A] = Nil
    for (x <- this)
      xs = x :: xs
    val b = bf(repr)
    for (x <- xs)
      b += f(x)

    b.result
  }

  /** An iterator yielding elements in reversed order.
   *
   *   $willNotTerminateInf
   *
   * Note: `xs.reverseIterator` is the same as `xs.reverse.iterator` but might be more efficient.
   *
   *  @return  an iterator yielding the elements of this $coll in reversed order
   */
  def reverseIterator: Iterator[A] = toCollection(reverse).iterator

  def startsWith[B](that: Seq[B], offset: Int): Boolean = {
    val i = this.iterator drop offset
    val j = that.iterator
    while (j.hasNext && i.hasNext)
      if (i.next != j.next)
        return false

    !j.hasNext
  }

  def endsWith[B](that: Seq[B]): Boolean = {
    val i = this.iterator.drop(length - that.length)
    val j = that.iterator
    while (i.hasNext && j.hasNext)
      if (i.next != j.next)
        return false

    !j.hasNext
  }

  /** Finds first index where this $coll contains a given sequence as a slice.
   *  $mayNotTerminateInf
   *  @param  that    the sequence to test
   *  @return  the first index such that the elements of this $coll starting at this index
   *           match the elements of sequence `that`, or `-1` of no such subsequence exists.
   */
  def indexOfSlice[B >: A](that: Seq[B]): Int = indexOfSlice(that, 0)

  /** Finds first index after or at a start index where this $coll contains a given sequence as a slice.
   *  $mayNotTerminateInf
   *  @param  that    the sequence to test
   *  @param  from    the start index
   *  @return  the first index `>= from` such that the elements of this $coll starting at this index
   *           match the elements of sequence `that`, or `-1` of no such subsequence exists.
   */
  def indexOfSlice[B >: A](that: Seq[B], from: Int): Int =
    if (this.hasDefiniteSize && that.hasDefiniteSize) {
      val l = length
      val tl = that.length
      val clippedFrom = math.max(0, from)
      if (from > l) -1
      else if (tl < 1) clippedFrom
      else if (l < tl) -1
      else SeqLike.kmpSearch(thisCollection, clippedFrom, l, that, 0, tl, true)
    }
    else {
      var i = from
      var s: Seq[A] = thisCollection drop i
      while (!s.isEmpty) {
        if (s startsWith that)
          return i

        i += 1
        s = s.tail
      }
      -1
    }

  /** Finds last index where this $coll contains a given sequence as a slice.
   *  $willNotTerminateInf
   *  @param  that    the sequence to test
   *  @return  the last index such that the elements of this $coll starting a this index
   *           match the elements of sequence `that`, or `-1` of no such subsequence exists.
   */
  def lastIndexOfSlice[B >: A](that: Seq[B]): Int = lastIndexOfSlice(that, length)

  /** Finds last index before or at a given end index where this $coll contains a given sequence as a slice.
   *  @param  that    the sequence to test
   *  @param  end     the end index
   *  @return  the last index `<= end` such that the elements of this $coll starting at this index
   *           match the elements of sequence `that`, or `-1` of no such subsequence exists.
   */
  def lastIndexOfSlice[B >: A](that: Seq[B], end: Int): Int = {
    val l = length
    val tl = that.length
    val clippedL = math.min(l-tl, end)

    if (end < 0) -1
    else if (tl < 1) clippedL
    else if (l < tl) -1
    else SeqLike.kmpSearch(thisCollection, 0, clippedL+tl, that, 0, tl, false)
  }

  /** Tests whether this $coll contains a given sequence as a slice.
   *  $mayNotTerminateInf
   *  @param  that    the sequence to test
   *  @return  `true` if this $coll contains a slice with the same elements
   *           as `that`, otherwise `false`.
   */
  def containsSlice[B](that: Seq[B]): Boolean = indexOfSlice(that) != -1

  /** Tests whether this $coll contains a given value as an element.
   *  $mayNotTerminateInf
   *
   *  @param elem  the element to test.
   *  @return     `true` if this $coll has an element that is equal (as
   *              determined by `==`) to `elem`, `false` otherwise.
   */
  def contains(elem: Any): Boolean = exists (_ == elem)

  /** Computes the multiset difference between this $coll and another sequence.
   *
   *  @param that   the sequence of elements to remove
   *  @tparam B     the element type of the returned $coll.
   *  @return       a new collection of type `That` which contains all elements of this $coll
   *                except some of occurrences of elements that also appear in `that`.
   *                If an element value `x` appears
   *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will not form
   *                part of the result, but any following occurrences will.
   *  @usecase def diff(that: Seq[A]): $Coll[A]
   *    @inheritdoc
   *
   *    $willNotTerminateInf
   *
   *    @return       a new $coll which contains all elements of this $coll
   *                  except some of occurrences of elements that also appear in `that`.
   *                  If an element value `x` appears
   *                  ''n'' times in `that`, then the first ''n'' occurrences of `x` will not form
   *                  part of the result, but any following occurrences will.
   */
  def diff[B >: A](that: Seq[B]): Repr = {
    val occ = occCounts(that)
    val b = newBuilder
    for (x <- this)
      if (occ(x) == 0) b += x
      else occ(x) -= 1
    b.result
  }

  /** Computes the multiset intersection between this $coll and another sequence.
   *
   *  @param that   the sequence of elements to intersect with.
   *  @tparam B     the element type of the returned $coll.
   *  @return       a new collection of type `That` which contains all elements of this $coll
   *                which also appear in `that`.
   *                If an element value `x` appears
   *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will be retained
   *                in the result, but any following occurrences will be omitted.
   *  @usecase def intersect(that: Seq[A]): $Coll[A]
   *    @inheritdoc
   *
   *    $mayNotTerminateInf
   *
   *    @return       a new $coll which contains all elements of this $coll
   *                  which also appear in `that`.
   *                  If an element value `x` appears
   *                  ''n'' times in `that`, then the first ''n'' occurrences of `x` will be retained
   *                  in the result, but any following occurrences will be omitted.
   */
  def intersect[B >: A](that: Seq[B]): Repr = {
    val occ = occCounts(that)
    val b = newBuilder
    for (x <- this)
      if (occ(x) > 0) {
        b += x
        occ(x) -= 1
      }
    b.result
  }

  private def occCounts[B](sq: Seq[B]): mutable.Map[B, Int] = {
    val occ = new mutable.HashMap[B, Int] { override def default(k: B) = 0 }
    for (y <- sq) occ(y) += 1
    occ
  }

  /** Builds a new $coll from this $coll without any duplicate elements.
   *  $willNotTerminateInf
   *
   *  @return  A new $coll which contains the first occurrence of every element of this $coll.
   */
  def distinct: Repr = {
    val b = newBuilder
    val seen = mutable.HashSet[A]()
    for (x <- this) {
      if (!seen(x)) {
        b += x
        seen += x
      }
    }
    b.result
  }

  def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    val (prefix, rest) = this.splitAt(index)
    b ++= toCollection(prefix)
    b += elem
    b ++= toCollection(rest).iterator.drop(1)
    b.result
  }

  def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    b += elem
    b ++= thisCollection
    b.result
  }

  def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    b ++= thisCollection
    b += elem
    b.result
  }

  def corresponds[B](that: Seq[B])(p: (A,B) => Boolean): Boolean = {
    val i = this.iterator
    val j = that.iterator
    while (i.hasNext && j.hasNext)
      if (!p(i.next, j.next))
        return false

    !i.hasNext && !j.hasNext
  }

  /** Sorts this $coll according to a comparison function.
   *  $willNotTerminateInf
   *
   *  The sort is stable. That is, elements that are equal (as determined by
   *  `lt`) appear in the same order in the sorted sequence as in the original.
   *
   *  @param  lt  the comparison function which tests whether
   *              its first argument precedes its second argument in
   *              the desired ordering.
   *  @return     a $coll consisting of the elements of this $coll
   *              sorted according to the comparison function `lt`.
   *  @example {{{
   *    List("Steve", "Tom", "John", "Bob").sortWith(_.compareTo(_) < 0) =
   *    List("Bob", "John", "Steve", "Tom")
   *  }}}
   */
  def sortWith(lt: (A, A) => Boolean): Repr = sorted(Ordering fromLessThan lt)

  /** Sorts this $Coll according to the Ordering which results from transforming
   *  an implicitly given Ordering with a transformation function.
   *  @see [[scala.math.Ordering]]
   *  $willNotTerminateInf
   *  @param   f the transformation function mapping elements
   *           to some other domain `B`.
   *  @param   ord the ordering assumed on domain `B`.
   *  @tparam  B the target type of the transformation `f`, and the type where
   *           the ordering `ord` is defined.
   *  @return  a $coll consisting of the elements of this $coll
   *           sorted according to the ordering where `x < y` if
   *           `ord.lt(f(x), f(y))`.
   *
   *  @example {{{
   *    val words = "The quick brown fox jumped over the lazy dog".split(' ')
   *    // this works because scala.Ordering will implicitly provide an Ordering[Tuple2[Int, Char]]
   *    words.sortBy(x => (x.length, x.head))
   *    res0: Array[String] = Array(The, dog, fox, the, lazy, over, brown, quick, jumped)
   *  }}}
   */
  def sortBy[B](f: A => B)(implicit ord: Ordering[B]): Repr = sorted(ord on f)

  /** Sorts this $coll according to an Ordering.
   *
   *  The sort is stable. That is, elements that are equal (as determined by
   *  `lt`) appear in the same order in the sorted sequence as in the original.
   *
   *  @see [[scala.math.Ordering]]
   *
   *  @param  ord the ordering to be used to compare elements.
   *  @return     a $coll consisting of the elements of this $coll
   *              sorted according to the ordering `ord`.
   */
  def sorted[B >: A](implicit ord: Ordering[B]): Repr = {
    val len = this.length
    val arr = new ArraySeq[A](len)
    var i = 0
    for (x <- this) {
      arr(i) = x
      i += 1
    }
    java.util.Arrays.sort(arr.array, ord.asInstanceOf[Ordering[Object]])
    val b = newBuilder
    b.sizeHint(len)
    for (x <- arr) b += x
    b.result
  }

  /** Converts this $coll to a sequence.
   *  $willNotTerminateInf
   *
   *  Overridden for efficiency.
   */
  override def toSeq: Seq[A] = thisCollection

  /** Produces the range of all indices of this sequence.
   *
   *  @return  a `Range` value from `0` to one less than the length of this $coll.
   */
  def indices: Range = 0 until length

  /* Need to override string, so that it's not the Function1's string that gets mixed in.
   */
  override def toString = super[IterableLike].toString
}

/** The companion object for trait `SeqLike`.
 */
object SeqLike {
  // KMP search utilities

  /** Make sure a target sequence has fast, correctly-ordered indexing for KMP.
   *
   *  @author Rex Kerr
   *  @since  2.10
   *  @param  W    The target sequence
   *  @param  n0   The first element in the target sequence that we should use
   *  @param  n1   The far end of the target sequence that we should use (exclusive)
   *  @return Target packed in an IndexedSeq (taken from iterator unless W already is an IndexedSeq)
   */
  private def kmpOptimizeWord[B](W: Seq[B], n0: Int, n1: Int, forward: Boolean) = W match {
    case iso: IndexedSeq[_] =>
      // Already optimized for indexing--use original (or custom view of original)
      if (forward && n0==0 && n1==W.length) iso.asInstanceOf[IndexedSeq[B]]
      else if (forward) new AbstractSeq[B] with IndexedSeq[B] {
        val length = n1 - n0
        def apply(x: Int) = iso(n0 + x).asInstanceOf[B]
      }
      else new AbstractSeq[B] with IndexedSeq[B] {
        def length = n1 - n0
        def apply(x: Int) = iso(n1 - 1 - x).asInstanceOf[B]
      }
    case _ =>
      // W is probably bad at indexing.  Pack in array (in correct orientation)
      // Would be marginally faster to special-case each direction
      new AbstractSeq[B] with IndexedSeq[B] {
        private[this] val Warr = new Array[AnyRef](n1-n0)
        private[this] val delta = if (forward) 1 else -1
        private[this] val done = if (forward) n1-n0 else -1
        val wit = W.iterator.drop(n0)
        var i = if (forward) 0 else (n1-n0-1)
        while (i != done) {
          Warr(i) = wit.next.asInstanceOf[AnyRef]
          i += delta
        }

        val length = n1 - n0
        def apply(x: Int) = Warr(x).asInstanceOf[B]
      }
  }

 /** Make a jump table for KMP search.
   *
   *  @author paulp, Rex Kerr
   *  @since  2.10
   *  @param  Wopt The target sequence, as at least an IndexedSeq
   *  @param  wlen Just in case we're only IndexedSeq and not IndexedSeqOptimized
   *  @return KMP jump table for target sequence
   */
 private def kmpJumpTable[B](Wopt: IndexedSeq[B], wlen: Int) = {
    val arr = new Array[Int](wlen)
    var pos = 2
    var cnd = 0
    arr(0) = -1
    arr(1) = 0
    while (pos < wlen) {
      if (Wopt(pos-1) == Wopt(cnd)) {
        arr(pos) = cnd + 1
        pos += 1
        cnd += 1
      }
      else if (cnd > 0) {
        cnd = arr(cnd)
      }
      else {
        arr(pos) = 0
        pos += 1
      }
    }
    arr
  }

 /**  A KMP implementation, based on the undoubtedly reliable wikipedia entry.
   *  Note: I made this private to keep it from entering the API.  That can be reviewed.
   *
   *  @author paulp, Rex Kerr
   *  @since  2.10
   *  @param  S       Sequence that may contain target
   *  @param  m0      First index of S to consider
   *  @param  m1      Last index of S to consider (exclusive)
   *  @param  W       Target sequence
   *  @param  n0      First index of W to match
   *  @param  n1      Last index of W to match (exclusive)
   *  @param  forward Direction of search (from beginning==true, from end==false)
   *  @return Index of start of sequence if found, -1 if not (relative to beginning of S, not m0).
   */
  private def kmpSearch[B](S: Seq[B], m0: Int, m1: Int, W: Seq[B], n0: Int, n1: Int, forward: Boolean): Int = {
    // Check for redundant case when target has single valid element
    @inline def clipR(x: Int, y: Int) = if (x<y) x else -1
    @inline def clipL(x: Int, y: Int) = if (x>y) x else -1

    if (n1 == n0+1) {
      if (forward)
        clipR(S.indexOf(W(n0), m0), m1)
      else
        clipL(S.lastIndexOf(W(n0), m1-1), m0-1)
    }

    // Check for redundant case when both sequences are same size
    else if (m1-m0 == n1-n0) {
      // Accepting a little slowness for the uncommon case.
      if (S.iterator.slice(m0, m1) == W.iterator.slice(n0, n1)) m0
      else -1
    }
    // Now we know we actually need KMP search, so do it
    else S match {
      case xs: IndexedSeq[_] =>
        // We can index into S directly; it should be adequately fast
        val Wopt = kmpOptimizeWord(W, n0, n1, forward)
        val T = kmpJumpTable(Wopt, n1-n0)
        var i, m = 0
        val zero = if (forward) m0 else m1-1
        val delta = if (forward) 1 else -1
        while (i+m < m1-m0) {
          if (Wopt(i) == S(zero+delta*(i+m))) {
            i += 1
            if (i == n1-n0) return (if (forward) m+m0 else m1-m-i)
          }
          else {
            val ti = T(i)
            m += i - ti
            if (i > 0) i = ti
          }
        }
        -1
      case _ =>
        // We had better not index into S directly!
        val iter = S.iterator.drop(m0)
        val Wopt = kmpOptimizeWord(W, n0, n1, true)
        val T = kmpJumpTable(Wopt, n1-n0)
        var cache = new Array[AnyRef](n1-n0)  // Ring buffer--need a quick way to do a look-behind
        var largest = 0
        var i, m = 0
        var answer = -1
        while (m+m0+n1-n0 <= m1) {
          while (i+m >= largest) {
            cache(largest%(n1-n0)) = iter.next.asInstanceOf[AnyRef]
            largest += 1
          }
          if (Wopt(i) == cache((i+m)%(n1-n0))) {
            i += 1
            if (i == n1-n0) {
              if (forward) return m+m0
              else {
                i -= 1
                answer = m+m0
                val ti = T(i)
                m += i - ti
                if (i > 0) i = ti
              }
            }
          }
          else {
            val ti = T(i)
            m += i - ti
            if (i > 0) i = ti
          }
        }
        answer
    }
  }

  /** Finds a particular index at which one sequence occurs in another sequence.
   *  Both the source sequence and the target sequence are expressed in terms
   *  other sequences S' and T' with offset and length parameters.  This
   *  function is designed to wrap the KMP machinery in a sufficiently general
   *  way that all library sequence searches can use it.  It is unlikely you
   *  have cause to call it directly: prefer functions such as StringBuilder#indexOf
   *  and Seq#lastIndexOf.
   *
   *  @param  source        the sequence to search in
   *  @param  sourceOffset  the starting offset in source
   *  @param  sourceCount   the length beyond sourceOffset to search
   *  @param  target        the sequence being searched for
   *  @param  targetOffset  the starting offset in target
   *  @param  targetCount   the length beyond targetOffset which makes up the target string
   *  @param  fromIndex     the smallest index at which the target sequence may start
   *
   *  @return the applicable index in source where target exists, or -1 if not found
   */
  def indexOf[B](
    source: Seq[B], sourceOffset: Int, sourceCount: Int,
    target: Seq[B], targetOffset: Int, targetCount: Int,
    fromIndex: Int
  ): Int = {
    // Fiddle with variables to match previous behavior and use kmpSearch
    // Doing LOTS of max/min, both clearer and faster to use math._
    val slen        = source.length
    val clippedFrom = math.max(0, fromIndex)
    val s0          = math.min(slen, sourceOffset + clippedFrom)
    val s1          = math.min(slen, s0 + sourceCount)
    val tlen        = target.length
    val t0          = math.min(tlen, targetOffset)
    val t1          = math.min(tlen, t0 + targetCount)

    // Error checking
    if (clippedFrom > slen-sourceOffset) -1   // Cannot return an index in range
    else if (t1 - t0 < 1) s0                  // Empty, matches first available position
    else if (s1 - s0 < t1 - t0) -1            // Source is too short to find target
    else {
      // Nontrivial search
      val ans = kmpSearch(source, s0, s1, target, t0, t1, true)
      if (ans < 0) ans else ans - math.min(slen, sourceOffset)
    }
  }

  /** Finds a particular index at which one sequence occurs in another sequence.
   *  Like `indexOf`, but finds the latest occurrence rather than earliest.
   *
   *  @see  [[scala.collection.SeqLike], method `indexOf`
   */
  def lastIndexOf[B](
    source: Seq[B], sourceOffset: Int, sourceCount: Int,
    target: Seq[B], targetOffset: Int, targetCount: Int,
    fromIndex: Int
  ): Int = {
    // Fiddle with variables to match previous behavior and use kmpSearch
    // Doing LOTS of max/min, both clearer and faster to use math._
    val slen        = source.length
    val tlen        = target.length
    val s0          = math.min(slen, sourceOffset)
    val s1          = math.min(slen, s0 + sourceCount)
    val clippedFrom = math.min(s1 - s0, fromIndex)
    val t0          = math.min(tlen, targetOffset)
    val t1          = math.min(tlen, t0 + targetCount)
    val fixed_s1    = math.min(s1, s0 + clippedFrom + (t1 - t0) - 1)

    // Error checking
    if (clippedFrom < 0) -1                   // Cannot return an index in range
    else if (t1 - t0 < 1) s0+clippedFrom      // Empty, matches last available position
    else if (fixed_s1 - s0 < t1 - t0) -1      // Source is too short to find target
    else {
      // Nontrivial search
      val ans = kmpSearch(source, s0, fixed_s1, target, t0, t1, false)
      if (ans < 0) ans else ans - s0
    }
  }
}
