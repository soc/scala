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
object LineNumber extends (Int => LineNumber) {
  final val Max = Int.MaxValue
  def apply(value: Int): LineNumber = new LineNumber(boundsCheck(1, Max)(value))
}

final class Index private (val value: Int) extends AnyVal with Ordered[Index] {
  def compare(that: Index): Int = (
    if (value < that.value) -1
    else if (value == that.value) 0
    else 1
  )
  def min(that: Index) = if (this < that) this else that
  def max(that: Index) = if (this > that) this else that
  def next: Index = Index(value + 1)
  def prev: Index = Index(value - 1)
  def add(n: Int): Index = Index(value + n)

  def until(end: Index): IndexRange = IndexRange(this, end)
  def to(end: Index): IndexRange    = this until end.next
  def selfRange: IndexRange         = this to this
  def emptyRange: IndexRange        = this until this

  override def toString = s"$value"
}

object Index extends (Int => Index) {
  final val Max = Int.MaxValue
  final val NoIndex = new Index(-1)

  def apply(value: Int): Index = new Index(boundsCheck(0, Max)(value))
}
