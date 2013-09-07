package scala
package reflect
package position

import JoinUnsignedShorts._

final class RowAndColumn private (val bits: Int) extends AnyVal with Ordered[RowAndColumn] {
  def isEmpty          = this == RowAndColumn.Empty
  def row: Int         = left(bits)
  def column: Int      = right(bits)
  def line: LineNumber = LineNumber(row)

  def compare(that: RowAndColumn) = (
    if (row < that.row) -1
    else if (that.row < row) 1
    else column compare that.column
  )
  override def toString = s"${row}x${column}"
}

object RowAndColumn {
  val Empty = new RowAndColumn(0)

  private def validate(value: Int) = (
    if (0 < value && value <= Int16) ()
    else illegalArg(s"Values must be between 1 and $Int16 (was: $value)")
  )

  def apply(row: Int, column: Int): RowAndColumn = {
    validate(row)
    validate(column)
    new RowAndColumn(join16(row, column))
  }
}
