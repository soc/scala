package scala
package reflect
package position

trait Positioned[T] extends Any {
  def pos: T
  def source: Source
  def coordinates: RowAndColumn
  def range: IndexRange
  def caratPoint: Index
  def isDefined: Boolean

  def line: Int
  def column: Int
  def start: Int
  def point: Int
  def end: Int

  def startOrElse(alt: Int): Int
  def pointOrElse(alt: Int): Int
  def endOrElse(alt: Int): Int

  def lineContent: String
  def lineCarat: String

  def coordinatesString: String
  def show: String
}

object Positioned {
  def apply[T](x: T)(implicit positioner: Positioner[T]): Positioned[T] = positioner positioned x
  def compat[T <: api.Position](x: T): Positioned[T] = apply(x)(CompatPositioner[T])
}
