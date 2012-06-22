/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import scala.collection.{ mutable, immutable, generic }
import scala.reflect.ClassTag
import scala.collection.mutable.{ ArrayLike, ArrayBuilder, ArrayOps, WrappedArray }
import immutable.{ StringOps, WrappedString }
import generic.CanBuildFrom
import annotation.{ elidable, implicitNotFound }
import annotation.elidable.ASSERTION
import language.{ implicitConversions, existentials }
import scala.runtime.{ ScalaRunTime, BoxedUnit }
import ScalaRunTime.arrayElementClass
import annotation.{ implicitWeight => weight }

/** The `Predef` object provides definitions that are accessible in all Scala
 *  compilation units without explicit qualification.
 *
 *  === Commonly Used Types ===
 *  Predef provides type aliases for types which are commonly used, such as
 *  the immutable collection types [[scala.collection.immutable.Map]],
 *  [[scala.collection.immutable.Set]], and the [[scala.collection.immutable.List]]
 *  constructors ([[scala.collection.immutable.::]] and
 *  [[scala.collection.immutable.Nil]]).
 *  The types `Pair` (a [[scala.Tuple2]]) and `Triple` (a [[scala.Tuple3]]), with
 *  simple constructors, are also provided.
 *
 *  === Console I/O ===
 *  Predef provides a number of simple functions for console I/O, such as
 *  `print`, `println`, `readLine`, `readInt`, etc. These functions are all
 *  aliases of the functions provided by [[scala.Console]].
 *
 *  === Assertions ===
 *
 *  A set of `assert` functions are provided for use as a way to document
 *  and dynamically check invariants in code. `assert` statements can be elided
 *  at runtime by providing the command line argument `-Xdisable-assertions` to
 *  the `scala` command.
 *
 *  Variants of `assert` intended for use with static analysis tools are also
 *  provided: `assume`, `require` and `ensuring`. `require` and `ensuring` are
 *  intended for use as a means of design-by-contract style specification
 *  of pre- and post-conditions on functions, with the intention that these
 *  specifications could be consumed by a static analysis tool. For instance,
 *
 *  {{{
 *  def addNaturals(nats: List[Int]): Int = {
 *    require(nats forall (_ >= 0), "List contains negative numbers")
 *    nats.foldLeft(0)(_ + _)
 *  } ensuring(_ >= 0)
 *  }}}
 *
 *  The declaration of `addNaturals` states that the list of integers passed should
 *  only contain natural numbers (i.e. non-negative), and that the result returned
 *  will also be natural. `require` is distinct from `assert` in that if the
 *  condition fails, then the caller of the function is to blame rather than a
 *  logical error having been made within `addNaturals` itself. `ensures` is a
 *  form of `assert` that declares the guarantee the function is providing with
 *  regards to it's return value.
 *
 *  === Implicit Conversions ===
 *  A number of commonly applied implicit conversions are also defined here, and
 *  in the parent type [[scala.LowPriorityImplicits]]. Implicit conversions
 *  are provided for the "widening" of numeric values, for instance, converting a
 *  Short value to a Long value as required, and to add additional higher-order
 *  functions to Array values. These are described in more detail in the documentation of [[scala.Array]].
 */
object Predef {
  import scala.collection.mutable.{ ArrayLike, ArrayBuilder, ArrayOps, WrappedArray }

  /** A class of `ArrayOps` for arrays containing reference types. */
  @inline implicit final class ofRef[T <: AnyRef](override val repr: Array[T]) extends AnyVal with ArrayOps[T] with ArrayLike[T, Array[T]] {
    override protected[this] def thisCollection: WrappedArray[T] = new WrappedArray.ofRef[T](repr)
    override protected[this] def toCollection(repr: Array[T]): WrappedArray[T] = new WrappedArray.ofRef[T](repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofRef[T]()(ClassTag[T](arrayElementClass(repr.getClass)))
    override def foreach[U](f: T => U) { var i = 0 ; val len = length ; while (i < len) { f(this(i)); i += 1 } }

    def length: Int = repr.length
    def apply(index: Int): T = repr(index)
    def update(index: Int, elem: T) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays containing `byte`s. */
  @inline implicit final class ofByte(override val repr: Array[Byte]) extends AnyVal with ArrayOps[Byte] with ArrayLike[Byte, Array[Byte]] {

    override protected[this] def thisCollection: WrappedArray[Byte] = new WrappedArray.ofByte(repr)
    override protected[this] def toCollection(repr: Array[Byte]): WrappedArray[Byte] = new WrappedArray.ofByte(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofByte
    override def foreach[U](f: Byte => U) { var i = 0 ; val len = length ; while (i < len) { f(this(i)); i += 1 } }

    def length: Int = repr.length
    def apply(index: Int): Byte = repr(index)
    def update(index: Int, elem: Byte) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays containing `short`s. */
  @inline implicit final class ofShort(override val repr: Array[Short]) extends AnyVal with ArrayOps[Short] with ArrayLike[Short, Array[Short]] {

    override protected[this] def thisCollection: WrappedArray[Short] = new WrappedArray.ofShort(repr)
    override protected[this] def toCollection(repr: Array[Short]): WrappedArray[Short] = new WrappedArray.ofShort(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofShort
    override def foreach[U](f: Short => U) { var i = 0 ; val len = length ; while (i < len) { f(this(i)); i += 1 } }

    def length: Int = repr.length
    def apply(index: Int): Short = repr(index)
    def update(index: Int, elem: Short) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays containing `char`s. */
  @inline implicit final class ofChar(override val repr: Array[Char]) extends AnyVal with ArrayOps[Char] with ArrayLike[Char, Array[Char]] {

    override protected[this] def thisCollection: WrappedArray[Char] = new WrappedArray.ofChar(repr)
    override protected[this] def toCollection(repr: Array[Char]): WrappedArray[Char] = new WrappedArray.ofChar(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofChar
    override def foreach[U](f: Char => U) { var i = 0 ; val len = length ; while (i < len) { f(this(i)); i += 1 } }

    def length: Int = repr.length
    def apply(index: Int): Char = repr(index)
    def update(index: Int, elem: Char) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays containing `int`s. */
  @inline implicit final class ofInt(override val repr: Array[Int]) extends AnyVal with ArrayOps[Int] with ArrayLike[Int, Array[Int]] {

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
  @inline implicit final class ofLong(override val repr: Array[Long]) extends AnyVal with ArrayOps[Long] with ArrayLike[Long, Array[Long]] {

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
  @inline implicit final class ofFloat(override val repr: Array[Float]) extends AnyVal with ArrayOps[Float] with ArrayLike[Float, Array[Float]] {

    override protected[this] def thisCollection: WrappedArray[Float] = new WrappedArray.ofFloat(repr)
    override protected[this] def toCollection(repr: Array[Float]): WrappedArray[Float] = new WrappedArray.ofFloat(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofFloat
    override def foreach[U](f: Float => U) { var i = 0 ; val len = length ; while (i < len) { f(this(i)); i += 1 } }

    def length: Int = repr.length
    def apply(index: Int): Float = repr(index)
    def update(index: Int, elem: Float) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays containing `double`s. */
  @inline implicit final class ofDouble(override val repr: Array[Double]) extends AnyVal with ArrayOps[Double] with ArrayLike[Double, Array[Double]] {
    override protected[this] def thisCollection: WrappedArray[Double] = new WrappedArray.ofDouble(repr)
    override protected[this] def toCollection(repr: Array[Double]): WrappedArray[Double] = new WrappedArray.ofDouble(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofDouble
    override def foreach[U](f: Double => U) { var i = 0 ; val len = length ; while (i < len) { f(this(i)); i += 1 } }

    def length: Int = repr.length
    def apply(index: Int): Double = repr(index)
    def update(index: Int, elem: Double) { repr(index) = elem }
  }

  /** A class of `ArrayOps` for arrays containing `boolean`s. */
  @inline implicit final class ofBoolean(override val repr: Array[Boolean]) extends AnyVal with ArrayOps[Boolean] with ArrayLike[Boolean, Array[Boolean]] {

    override protected[this] def thisCollection: WrappedArray[Boolean] = new WrappedArray.ofBoolean(repr)
    override protected[this] def toCollection(repr: Array[Boolean]): WrappedArray[Boolean] = new WrappedArray.ofBoolean(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofBoolean
    override def foreach[U](f: Boolean => U) { var i = 0 ; val len = length ; while (i < len) { f(this(i)); i += 1 } }

    def length: Int = repr.length
    def apply(index: Int): Boolean = repr(index)
    def update(index: Int, elem: Boolean) { repr(index) = elem }
  }

  /** Former LowPriorityImplicits **/

  /** We prefer the java.lang.* boxed types to these wrappers in
   *  any potential conflicts.  Conflicts do exist because the wrappers
   *  need to implement ScalaNumber in order to have a symmetric equals
   *  method, but that implies implementing java.lang.Number as well.
   */
  @weight(-1) implicit def intWrapper(x: Int)         = new runtime.RichInt(x)
  @weight(-1) implicit def charWrapper(c: Char)       = new runtime.RichChar(c)
  @weight(-1) implicit def longWrapper(x: Long)       = new runtime.RichLong(x)
  @weight(-1) implicit def floatWrapper(x: Float)     = new runtime.RichFloat(x)
  @weight(-1) implicit def doubleWrapper(x: Double)   = new runtime.RichDouble(x)

  // These eight implicits exist solely to exclude Null from the domain of
  // the boxed types, so that e.g. "var x: Int = null" is a compile time
  // error rather than a delayed null pointer exception by way of the
  // conversion from java.lang.Integer.  If defined in the same file as
  // Integer2int, they would have higher priority because Null is a subtype
  // of Integer.  We balance that out and create conflict by moving the
  // definition into the superclass.
  //
  // Caution: do not adjust tightrope tension without safety goggles in place.
  @weight(-1) implicit def Byte2byteNullConflict(x: Null): Byte          = sys.error("value error")
  @weight(-1) implicit def Short2shortNullConflict(x: Null): Short       = sys.error("value error")
  @weight(-1) implicit def Character2charNullConflict(x: Null): Char     = sys.error("value error")
  @weight(-1) implicit def Integer2intNullConflict(x: Null): Int         = sys.error("value error")
  @weight(-1) implicit def Long2longNullConflict(x: Null): Long          = sys.error("value error")
  @weight(-1) implicit def Float2floatNullConflict(x: Null): Float       = sys.error("value error")
  @weight(-1) implicit def Double2doubleNullConflict(x: Null): Double    = sys.error("value error")
  @weight(-1) implicit def Boolean2booleanNullConflict(x: Null): Boolean = sys.error("value error")

  @weight(-1) implicit def genericWrapArray[T](xs: Array[T]): WrappedArray[T] =
    if (xs eq null) null
    else WrappedArray.make(xs)

  // Since the JVM thinks arrays are covariant, one 0-length Array[AnyRef]
  // is as good as another for all T <: AnyRef.  Instead of creating 100,000,000
  // unique ones by way of this implicit, let's share one.
  @weight(-1) implicit def wrapRefArray[T <: AnyRef](xs: Array[T]): WrappedArray[T] = {
    if (xs eq null) null
    else if (xs.length == 0) WrappedArray.empty[T]
    else new WrappedArray.ofRef[T](xs)
  }

  @weight(-1) implicit def wrapIntArray(xs: Array[Int]): WrappedArray[Int] = if (xs ne null) new WrappedArray.ofInt(xs) else null
  @weight(-1) implicit def wrapDoubleArray(xs: Array[Double]): WrappedArray[Double] = if (xs ne null) new WrappedArray.ofDouble(xs) else null
  @weight(-1) implicit def wrapLongArray(xs: Array[Long]): WrappedArray[Long] = if (xs ne null) new WrappedArray.ofLong(xs) else null
  @weight(-1) implicit def wrapFloatArray(xs: Array[Float]): WrappedArray[Float] = if (xs ne null) new WrappedArray.ofFloat(xs) else null
  @weight(-1) implicit def wrapCharArray(xs: Array[Char]): WrappedArray[Char] = if (xs ne null) new WrappedArray.ofChar(xs) else null
  @weight(-1) implicit def wrapByteArray(xs: Array[Byte]): WrappedArray[Byte] = if (xs ne null) new WrappedArray.ofByte(xs) else null
  @weight(-1) implicit def wrapShortArray(xs: Array[Short]): WrappedArray[Short] = if (xs ne null) new WrappedArray.ofShort(xs) else null
  @weight(-1) implicit def wrapBooleanArray(xs: Array[Boolean]): WrappedArray[Boolean] = if (xs ne null) new WrappedArray.ofBoolean(xs) else null

  @weight(-1) implicit def wrapString(s: String): WrappedString = if (s ne null) new WrappedString(s) else null
  @weight(-1) implicit def unwrapString(ws: WrappedString): String = if (ws ne null) ws.self else null

  @weight(-1) implicit def fallbackStringCanBuildFrom[T]: CanBuildFrom[String, T, immutable.IndexedSeq[T]] =
    new CanBuildFrom[String, T, immutable.IndexedSeq[T]] {
      def apply(from: String) = immutable.IndexedSeq.newBuilder[T]
      def apply() = immutable.IndexedSeq.newBuilder[T]
    }

  /** End LowPriorityImplicits **/

  /**
   * Retrieve the runtime representation of a class type. `classOf[T]` is equivalent to
   * the class literal `T.class` in Java.
   *
   * @example {{{
   * val listClass = classOf[List[_]]
   * // listClass is java.lang.Class[List[_]] = class scala.collection.immutable.List
   *
   * val mapIntString = classOf[Map[Int,String]]
   * // mapIntString is java.lang.Class[Map[Int,String]] = interface scala.collection.immutable.Map
   * }}}
   */
  def classOf[T]: Class[T] = null // This is a stub method. The actual implementation is filled in by the compiler.

  type String        = java.lang.String
  type Class[T]      = java.lang.Class[T]

  // miscelleaneous -----------------------------------------------------
  scala.`package`                         // to force scala package object to be seen.
  scala.collection.immutable.List         // to force Nil, :: to be seen.

  type Function[-A, +B] = Function1[A, B]

  type Map[A, +B] = immutable.Map[A, B]
  type Set[A]     = immutable.Set[A]
  val Map         = immutable.Map
  val Set         = immutable.Set
  // @deprecated("Use scala.AnyRef instead", "2.10.0")
  // def AnyRef = scala.AnyRef

  // Manifest types, companions, and incantations for summoning
  type ClassManifest[T] = scala.reflect.ClassManifest[T]
  type OptManifest[T]   = scala.reflect.OptManifest[T]
  type Manifest[T]      = scala.reflect.Manifest[T]
  val ClassManifest     = scala.reflect.ClassManifest
  val Manifest          = scala.reflect.Manifest
  val NoManifest        = scala.reflect.NoManifest

  def manifest[T](implicit m: Manifest[T])           = m
  def classManifest[T](implicit m: ClassManifest[T]) = m
  def optManifest[T](implicit m: OptManifest[T])     = m

  // Minor variations on identity functions
  def identity[A](x: A): A         = x    // @see `conforms` for the implicit version
  @inline def implicitly[T](implicit e: T) = e    // for summoning implicit values from the nether world -- TODO: when dependent method types are on by default, give this result type `e.type`, so that inliner has better chance of knowing which method to inline in calls like `implicitly[MatchingStrategy[Option]].zero`
  @inline def locally[T](x: T): T  = x    // to communicate intent and avoid unmoored statements

  @deprecated("Use sys.error(message) instead", "2.9.0")
  def error(message: String): Nothing = sys.error(message)

  // errors and asserts -------------------------------------------------

  /** Tests an expression, throwing an `AssertionError` if false.
   *  Calls to this method will not be generated if `-Xelide-below`
   *  is at least `ASSERTION`.
   *
   *  @see elidable
   *  @param assertion   the expression to test
   */
  @elidable(ASSERTION)
  def assert(assertion: Boolean) {
    if (!assertion)
      throw new java.lang.AssertionError("assertion failed")
  }

  /** Tests an expression, throwing an `AssertionError` if false.
   *  Calls to this method will not be generated if `-Xelide-below`
   *  is at least `ASSERTION`.
   *
   *  @see elidable
   *  @param assertion   the expression to test
   *  @param message     a String to include in the failure message
   */
  @elidable(ASSERTION) @inline
  final def assert(assertion: Boolean, message: => Any) {
    if (!assertion)
      throw new java.lang.AssertionError("assertion failed: "+ message)
  }

  /** Tests an expression, throwing an `AssertionError` if false.
   *  This method differs from assert only in the intent expressed:
   *  assert contains a predicate which needs to be proven, while
   *  assume contains an axiom for a static checker.  Calls to this method
   *  will not be generated if `-Xelide-below` is at least `ASSERTION`.
   *
   *  @see elidable
   *  @param assumption   the expression to test
   */
  @elidable(ASSERTION)
  def assume(assumption: Boolean) {
    if (!assumption)
      throw new java.lang.AssertionError("assumption failed")
  }

  /** Tests an expression, throwing an `AssertionError` if false.
   *  This method differs from assert only in the intent expressed:
   *  assert contains a predicate which needs to be proven, while
   *  assume contains an axiom for a static checker.  Calls to this method
   *  will not be generated if `-Xelide-below` is at least `ASSERTION`.
   *
   *  @see elidable
   *  @param assumption   the expression to test
   *  @param message      a String to include in the failure message
   */
  @elidable(ASSERTION) @inline
  final def assume(assumption: Boolean, message: => Any) {
    if (!assumption)
      throw new java.lang.AssertionError("assumption failed: "+ message)
  }

  /** Tests an expression, throwing an `IllegalArgumentException` if false.
   *  This method is similar to `assert`, but blames the caller of the method
   *  for violating the condition.
   *
   *  @param requirement   the expression to test
   */
  def require(requirement: Boolean) {
    if (!requirement)
      throw new IllegalArgumentException("requirement failed")
  }

  /** Tests an expression, throwing an `IllegalArgumentException` if false.
   *  This method is similar to `assert`, but blames the caller of the method
   *  for violating the condition.
   *
   *  @param requirement   the expression to test
   *  @param message       a String to include in the failure message
   */
  @inline final def require(requirement: Boolean, message: => Any) {
    if (!requirement)
      throw new IllegalArgumentException("requirement failed: "+ message)
  }

  /** `???` can be used for marking methods that remain to be implemented.
   *  @throws  A `NotImplementedError`
   */
  def ??? : Nothing = throw new NotImplementedError

  // tupling ------------------------------------------------------------

  type Pair[+A, +B] = Tuple2[A, B]
  object Pair {
    def apply[A, B](x: A, y: B) = Tuple2(x, y)
    def unapply[A, B](x: Tuple2[A, B]): Option[Tuple2[A, B]] = Some(x)
  }

  @inline implicit final class ArrowAssoc[A](val __leftOfArrow: A) extends AnyVal {
    @inline def -> [B](y: B): Tuple2[A, B] = Tuple2(__leftOfArrow, y)
    @inline def asAnyRef: AnyRef = __leftOfArrow.asInstanceOf[AnyRef]
  }

  // printing and reading -----------------------------------------------

  def print(x: Any)                  = Console.print(x)
  def println()                      = Console.println()
  def println(x: Any)                = Console.println(x)
  def printf(text: String, xs: Any*) = Console.print(text.format(xs: _*))

  // views --------------------------------------------------------------
  // 
  // implicit def arrayTagToClassManifest[T](elemTag: ArrayTag[T]): ClassManifest[T] =
  //   ClassManifest.fromClass[T](ScalaRunTime.arrayElementClass(elemTag).asInstanceOf[Class[T]])

  implicit def tuple2ToZippedOps[T1, T2](x: (T1, T2))                           = new runtime.Tuple2Zipped.Ops(x)
  implicit def seqToCharSequence(xs: collection.IndexedSeq[Char]): CharSequence = new runtime.SeqCharSequence(xs)
  implicit def arrayToCharSequence(xs: Array[Char]): CharSequence               = new runtime.ArrayCharSequence(xs, 0, xs.length)
  
  @inline implicit final class TapOps[T](val x: T) extends AnyVal {
    @inline def tap(f: T => Any): T = { f(x) ; x }
  }

  implicit def genericArrayOps[T](xs: Array[T]): ArrayOps[T] = (xs match {
    case x: Array[AnyRef]  => x: ArrayOps[AnyRef]
    case x: Array[Boolean] => x: ArrayOps[Boolean]
    case x: Array[Byte]    => x: ArrayOps[Byte]
    case x: Array[Char]    => x: ArrayOps[Char]
    case x: Array[Double]  => x: ArrayOps[Double]
    case x: Array[Float]   => x: ArrayOps[Float]
    case x: Array[Int]     => x: ArrayOps[Int]
    case x: Array[Long]    => x: ArrayOps[Long]
    case x: Array[Short]   => x: ArrayOps[Short]
    case x: Array[Unit]    => x: ArrayOps[Unit]
    case null              => null
  }).asInstanceOf[ArrayOps[T]]

  // "Autoboxing" and "Autounboxing" ---------------------------------------------------

  implicit def byte2Byte(x: Byte)           = java.lang.Byte.valueOf(x)
  implicit def short2Short(x: Short)        = java.lang.Short.valueOf(x)
  implicit def char2Character(x: Char)      = java.lang.Character.valueOf(x)
  implicit def int2Integer(x: Int)          = java.lang.Integer.valueOf(x)
  implicit def long2Long(x: Long)           = java.lang.Long.valueOf(x)
  implicit def float2Float(x: Float)        = java.lang.Float.valueOf(x)
  implicit def double2Double(x: Double)     = java.lang.Double.valueOf(x)
  implicit def boolean2Boolean(x: Boolean)  = java.lang.Boolean.valueOf(x)

  // These next eight implicits exist solely to exclude AnyRef methods from the
  // eight implicits above so that primitives are not coerced to AnyRefs.  They
  // only create such conflict for AnyRef methods, so the methods on the java.lang
  // boxed types are unambiguously reachable.
  implicit def byte2ByteConflict(x: Byte)           = new AnyRef
  implicit def short2ShortConflict(x: Short)        = new AnyRef
  implicit def char2CharacterConflict(x: Char)      = new AnyRef
  implicit def int2IntegerConflict(x: Int)          = new AnyRef
  implicit def long2LongConflict(x: Long)           = new AnyRef
  implicit def float2FloatConflict(x: Float)        = new AnyRef
  implicit def double2DoubleConflict(x: Double)     = new AnyRef
  implicit def boolean2BooleanConflict(x: Boolean)  = new AnyRef

  implicit def Byte2byte(x: java.lang.Byte): Byte             = x.byteValue
  implicit def Short2short(x: java.lang.Short): Short         = x.shortValue
  implicit def Character2char(x: java.lang.Character): Char   = x.charValue
  implicit def Integer2int(x: java.lang.Integer): Int         = x.intValue
  implicit def Long2long(x: java.lang.Long): Long             = x.longValue
  implicit def Float2float(x: java.lang.Float): Float         = x.floatValue
  implicit def Double2double(x: java.lang.Double): Double     = x.doubleValue
  implicit def Boolean2boolean(x: java.lang.Boolean): Boolean = x.booleanValue

  // Strings and CharSequences --------------------------------------------------------------

  @inline implicit def augmentString(x: String): StringOps = new StringOps(x)
  implicit def unaugmentString(x: StringOps): String = x.repr
  implicit def enrichClass[T](clazz: Class[T]) = new runtime.RichClass[T](clazz)

  implicit val StringCanBuildFrom: CanBuildFrom[String, Char, String] = new CanBuildFrom[String, Char, String] {
    def apply(from: String) = apply()
    def apply()             = mutable.StringBuilder.newBuilder
  }
  
  // Temporary
    
  @inline final def ultimately[T](fin: => Unit)(body: => T): T = try body finally fin

  // Type Constraints --------------------------------------------------------------

  /**
   * An instance of `A <:< B` witnesses that `A` is a subtype of `B`.
   * Requiring an implicit argument of the type `A <:< B` encodes
   * the generalized constraint `A <: B`.
   *
   * @note we need a new type constructor `<:<` and evidence `conforms`,
   * as reusing `Function1` and `identity` leads to ambiguities in
   * case of type errors (`any2stringadd` is inferred)
   *
   * To constrain any abstract type T that's in scope in a method's
   * argument list (not just the method's own type parameters) simply
   * add an implicit argument of type `T <:< U`, where `U` is the required
   * upper bound; or for lower-bounds, use: `L <:< T`, where `L` is the
   * required lower bound.
   *
   * In part contributed by Jason Zaugg.
   */
  @implicitNotFound(msg = "Cannot prove that ${From} <:< ${To}.")
  sealed abstract class <:<[-From, +To] extends (From => To) with Serializable
  private[this] final val singleton_<:< = new <:<[Any,Any] { def apply(x: Any): Any = x }
  // not in the <:< companion object because it is also
  // intended to subsume identity (which is no longer implicit)
  implicit def conforms[A]: A <:< A = singleton_<:<.asInstanceOf[A <:< A]

  /** An instance of `A =:= B` witnesses that the types `A` and `B` are equal.
   *
   * @see `<:<` for expressing subtyping constraints
   */
  @implicitNotFound(msg = "Cannot prove that ${From} =:= ${To}.")
  sealed abstract class =:=[From, To] extends (From => To) with Serializable
  private[this] final val singleton_=:= = new =:=[Any,Any] { def apply(x: Any): Any = x }
  object =:= {
     implicit def tpEquals[A]: A =:= A = singleton_=:=.asInstanceOf[A =:= A]
  }
}
