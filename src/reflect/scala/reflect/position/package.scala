package scala
package reflect

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
  type SourceFile      = scala.reflect.internal.util.SourceFile
  type BatchSourceFile = scala.reflect.internal.util.BatchSourceFile
  type RangePosition   = scala.reflect.internal.util.RangePosition
  type OffsetPosition  = scala.reflect.internal.util.OffsetPosition

  def newUri(path: String): Uri = if (path contains "://") new Uri(path) else (new File(path)).toURI



  def NoSourceId = SourceId.None
  def NoNodeId   = NodeId.None
  def NoTreeId   = TreeId.None
}