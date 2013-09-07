package scala
package reflect
package position

// import Index._

final class LineNumber private (val value: Int) extends AnyVal {
  def index: Int = value - 1
  def next: LineNumber = if (value == Int.MaxValue) illegalArg(s"$this.next") else LineNumber(value + 1)
  def prev: LineNumber = if (value <= 1) illegalArg(s"$this.prev") else LineNumber(value - 1)
  override def toString = s"line $value"
}
object LineNumber extends (Int => LineNumber) {
  def apply(value: Int): LineNumber = if (value <= 0) illegalArg(s"$value <= 0") else new LineNumber(value)
}

final class Index private (val value: Int) extends AnyVal with Ordered[Index] {
  def compare(that: Index): Int = (
    if (value < that.value) -1
    else if (value == that.value) 0
    else 1
  )
  def min(that: Index) = if (this < that) this else that
  def max(that: Index) = if (this > that) this else that
  def next: Index = if (value == Int.MaxValue) illegalArg(s"$this.next") else Index(value + 1)
  def prev: Index = if (value == 0) illegalArg(s"$this.prev") else Index(value - 1)
  def add(n: Int): Index = Index(value + n)

  def until(end: Index): IndexRange = IndexRange(this, end)
  def to(end: Index): IndexRange    = this until end.next
  def selfRange: IndexRange         = this to this
  def emptyRange: IndexRange        = this until this

  override def toString = s"$value"
}

object Index extends (Int => Index) {
  final val NoIndex = new Index(-1)

  def apply(value: Int): Index = if (value < 0) illegalArg(s"$value < 0") else new Index(value)
}
