package scala
package reflect
package position

class Sources(val sources: Vector[Source]) extends Source {
  private[this] var active: Source = NoSource

  def activeSource: Source = active

  def name                         = active.name
  def path                         = active.path
  def chars                        = active.chars
  def lines                        = active.lines
  def lineIndices                  = active.lineIndices
  def coordinatesOf(offset: Index) = active coordinatesOf offset

  def mapSources[T](f: Source => T): Vector[T] = sources map (s => withActiveSource(s)(f(s)))
  def foreachSource(f: Source => Unit): Unit = sources foreach (s => withActiveSource(s)(f(s)))

  @inline final def withActiveSource[T](s: Source)(body: => T): T = {
    val saved = active
    active = s
    try body finally active = saved
  }
}
