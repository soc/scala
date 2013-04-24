package scala
package math

final class Index private (val intValue: Int) extends AnyVal with Ordered[Index] {
  def compare(that: Index): Int = (
    if (intValue < that.intValue) -1
    else if (intValue == that.intValue) 0
    else 1
  )
  def max(that: Index): Index = (
    if (this.isEmpty || that.isEmpty) Index.None
    else Index(math.max(intValue, that.intValue))
  )
  def min(that: Index): Index = (
    if (this.isEmpty || that.isEmpty) Index.None
    else Index(math.min(intValue, that.intValue))
  )
  def +(amount: Int): Index = if (this.isEmpty) Index.None else Index(intValue + amount)
  def -(amount: Int): Index = if (this.isEmpty) Index.None else Index(intValue - amount)

  def toOption: Option[Int] = if (isEmpty) scala.None else Some(intValue)
  def orElse(alt: => Int): Int = if (isEmpty) alt else intValue

  def width(that: Index): Index = (
    if (this.isEmpty || that.isEmpty) Index.None
    else if (that.intValue <= intValue) Index(0)
    else Index(that.intValue - intValue)
  )
  def until(that: Index): Range = intValue until that.intValue

  def get = intValue
  def isEmpty = intValue < 0
  override def toString = if (isEmpty) "Index.None" else s"Index($intValue)"
}

object Index {
  final val None = new Index(-1)
  def apply(x: Int): Index = if (x < 0) None else new Index(x)
  // Requires pattern matcher isEmpty/get enhancement
  // def unapply(x: Index): Index = x

  implicit def liftIntToIndex(x: Int): Index = apply(x)
  implicit def lowerIndexToInt(x: Index): Int = x.intValue

  final class IndexOps private[Index] (val lhs: Int) extends AnyVal {
    def isEmpty = lhs < 0
    def +(rhs: Int): Index = if (isEmpty) None else Index(lhs + rhs)
    def -(rhs: Int): Index = if (isEmpty) None else Index(lhs - rhs)
  }
  final class IndexOps2 private[Index] (val lhs: Int) extends AnyVal {
    def isEmpty = lhs < 0
    def +(rhs: Index): Index = if (isEmpty) None else Index(lhs + rhs.intValue)
    def -(rhs: Index): Index = if (isEmpty) None else Index(lhs - rhs.intValue)
  }

  implicit def IndexOps(x: Index): IndexOps = new IndexOps(x.intValue)
  implicit def IndexOps2(x: Index): IndexOps2 = new IndexOps2(x.intValue)
}
