package scala
package reflect

import scala.reflect.internal.SymbolTable

package object position {
  private[position] implicit lazy val implicitConversions = scala.language.implicitConversions
  private[position] def boundsCheck(min: Int, max: Int)(arg: Int): Int = (
    if (min <= arg && arg <= max) arg
    else throw new IllegalArgumentException(s"Argument must be between $min and $max (was: $arg)")
  )

  /** Return a same-length string as the basis with all characters
   *  turned into spaces, except tabs which remain tabs.
   */
  private[position] def mapToWhitespace(basis: String): String =
    basis map (ch => if (ch == '\t') ch else ' ') mkString ""

  type File            = java.io.File
  type Url             = java.net.URL
  type Uri             = java.net.URI
  type PlainFile       = scala.reflect.io.PlainFile
  type SourceFile      = scala.reflect.internal.util.SourceFile
  type BatchSourceFile = scala.reflect.internal.util.BatchSourceFile
  type RangePosition   = scala.reflect.internal.util.RangePosition
  type OffsetPosition  = scala.reflect.internal.util.OffsetPosition
  type UtilPosition    = scala.reflect.internal.util.Position
  type ApiPosition     = scala.reflect.api.Position

  val Position = scala.reflect.internal.util.Position

  def newUri(path: String): Uri = if (path contains "://") new Uri(path) else (new File(path)).toURI

  implicit def sourceOps(source: Source): SourceOps                    = new SourceOps(source)
  implicit def sourceDebugOps(source: Source): SourceDebugOps          = new SourceDebugOps(source)
  implicit def sourcedPosOps[T: Positioner](entity: T): SourcedPosData = SourcedPosData(entity)

  def NoSourceId = SourceId.None
  def NoNodeId   = NodeId.None
  def NoTreeId   = TreeId.None
  def NoPosData  = PosData.None

  def newTreePositioner[U <: SymbolTable](u: U): Positioner[u.Tree] = new Positioner[u.Tree] {
    def positionFor(entity: u.Tree): PosData = PosData(entity.pos)
    def sourceFor(entity: u.Tree): Source    = u.sources(entity.id.sourceId)
  }
}
