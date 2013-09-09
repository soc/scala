package scala
package reflect
package position

trait Positioner[T] extends Any {
  def positionFor(entity: T): PosData
  def sourceFor(entity: T): Source
}
