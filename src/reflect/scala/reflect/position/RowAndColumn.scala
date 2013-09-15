package scala
package reflect
package position

import BitOps._

final class RowAndColumn private (val bits: Int) extends AnyVal with Ordered[RowAndColumn] {
  def isEmpty          = this == RowAndColumn.Empty
  def row: Int         = left16(bits)
  def column: Int      = right16(bits)
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
  private def check(arg: Int) = boundsCheck(1, Int16)(arg)
  def apply(row: Int, column: Int): RowAndColumn = new RowAndColumn(join32(check(row), check(column)))
}
