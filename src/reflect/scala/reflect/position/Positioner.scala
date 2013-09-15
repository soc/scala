package scala
package reflect
package position

trait Positioner[T] extends Any {
  def positionFor(entity: T): PosData
  def sourceFor(entity: T): Source
}

object CompatPositioner extends Positioner[api.Position] {
  def positionFor(entity: api.Position): PosData = PosData compat entity
  def sourceFor(entity: api.Position): Source = Source(entity.source)
}
