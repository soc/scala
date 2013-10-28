package scala
package reflect
package position

trait Positioner[T] extends Any {
  def sourceFor(pos: T): Source
  def rangeFor(pos: T): IndexRange
  def pointFor(pos: T): Index
  def isDefinedAt(pos: T): Boolean
  def undefined: T
  def positioned(pos: T): Positioned[T]
  def reposition(pos: T, range: IndexRange): T
}
