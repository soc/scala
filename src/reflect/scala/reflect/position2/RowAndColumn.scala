package scala
package reflect
package position

import JoinInts._

final class RowAndColumn private (val bits: Long) extends AnyVal with Ordered[RowAndColumn] {
  def isEmpty = this == RowAndColumn.Empty
  def row    = if (isEmpty) sys.error("<no row>") else left(bits)
  def column = if (isEmpty) sys.error("<no col>") else right(bits)

  // def row    = bits >> RowAndColumn.ColumnBits
  // def column = bits & RowAndColumn.ColumnMask
  def compare(that: RowAndColumn) = (
    if (row < that.row) -1
    else if (that.row < row) 1
    else column compare that.column
  )


   // ((row, column)) compare ((row, column))


  // row compare that.row match {
  //   case 0 => column compare that.column
  //   case x => x
  // }
  override def toString = s"${row}x${column}"
}

object RowAndColumn {
  val Empty = new RowAndColumn(0)
  def apply(row: Int, column: Int): RowAndColumn = new RowAndColumn(join(row, column))

  // final val ColumnBits = 9
  // final val ColumnMask = (1 << (ColumnBits + 1)) - 1
  // final val RowMask    = (-1 & ~ColumnMask) >> ColumnBits

  // def apply(row: Int, column: Int): RowAndColumn = new RowAndColumn(((row & RowMask) << ColumnBits) | (column & ColumnMask))
}
