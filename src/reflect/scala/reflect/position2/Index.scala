package scala
package reflect
package position

// import Index._

final class LineNumber private (val value: Int) extends AnyVal {
  def index: Int = value - 1
  def next: LineNumber = LineNumber(value + 1)
  def prev: LineNumber = LineNumber(value - 1)
  override def toString = s"line $value"
}
object LineNumber {
  def apply(value: Int): LineNumber = if (value <= 0) throw new IllegalArgumentException(s"line: $value") else new LineNumber(value)
}

final class Index[T] private (val value: Int) extends AnyVal with Ordered[Index[T]] {
  def index = value
  def compare(that: Index[T]): Int = (
    if (value < that.value) -1
    else if (value == that.value) 0
    else 1
  )
  def min(that: Index[T]) = if (this < that) this else that
  def max(that: Index[T]) = if (this > that) this else that
  def next: Index[T] = Index(value + 1)
  def prev: Index[T] = Index(value - 1)
  def add(n: Int): Index[T] = Index(value + n)

  def isValidForLength(length: Length[T]) = 0 <= value && value < length.value

  def to(end: Index[T]): IndexRange[T]        = IndexRange.inclusive(this, end)
  def until(end: Index[T]): IndexRange[T]     = IndexRange.exclusive(this, end)
  def steps(length: Length[T]): IndexRange[T] = IndexRange.steps(this, length)
  def selfRange: IndexRange[T]                = IndexRange.inclusive(this, this)
  def emptyRange: IndexRange[T]               = IndexRange.exclusive(this, this)

  override def toString = s"$value"
}

// final class Index[T] private (val bits: Int) extends AnyVal with Ordered[Index[T]] {
//   def compare(that: Index[T]): Int = (
//     if (index < that.index) -1
//     else if (index == that.index) 0
//     else 1
//   )

//   def min(that: Index[T]) = if (this < that) this else that
//   def max(that: Index[T]) = if (this > that) this else that

//   def isZeroBased = bits >= 0
//   def isNoIndex   = bits == -1
//   def isOneBased  = bits < -1

//   def index: Int        = if (isOneBased) uint31(bits + Int.MaxValue + 2) else bits
//   def next: Index[T] = shift(1)
//   def prev: Index[T] = shift(-1)

//   def isValidForLength(length: Length[T]): Boolean = (
//        (0 < index && index < length.value)
//     || (isZeroBased && index == 0)
//     || (isOneBased  && index == length.value)
//   )

//   def shift(n: Int): Index[T] = Index.fromBits[T](bits + n)

//   def to(end: Index[T]): IndexRange[T]        = IndexRange.inclusive(this, end)
//   def until(end: Index[T]): IndexRange[T]     = IndexRange.exclusive(this, end)
//   def steps(length: Length[T]): IndexRange[T] = IndexRange.steps(this, length)
//   def selfRange: IndexRange[T]                = IndexRange.inclusive(this, this)
//   def emptyRange: IndexRange[T]               = IndexRange.exclusive(this, this)

//   private def baseString = if (isZeroBased) "0" else "1"
//   override def toString = s"$index"
// }

object Index {
  final val NoIndex = new Index[Any](-1)
  // final val Line = new Object
  // final val Long32  = 0xFFFFFFFFL
  // final val Long31  = 0x7FFFFFFFL
  // final val Int32   = 0xFFFFFFFF
  // final val Int31   = 0x7FFFFFFF

  // def uint31(value: Int): Int  = value & Int31
  // def uint32(value: Int): Long = value & Long32
  // def hasBit(bits: Long, bit: Int): Boolean = (bits & (1L << bit)) != 0

  // private def join(lbits: Long, rbits: Long) = (lbits << 32) | rbits
  // def join(lbits: Int, rbits: Int): Long = join(lbits & Long32, rbits & Long32)
  // def left(joined: Long): Int            = (joined >>> 32).toInt & Int32
  // def right(joined: Long): Int           = joined.toInt

  // final val Line     = new Object
  // final val Column   = new Object

  private def fail(index: Int) = throw new IllegalArgumentException(s"index: $index")
  def apply[T](index: Int)     = if (index < 0) fail(index) else new Index[T](index)
}
