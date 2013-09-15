package scala
package reflect

import scala.reflect.internal.util._
// import scala.language.implicitConversions

package object position {
  private[position] implicit lazy val implicitConversions = scala.language.implicitConversions

  implicit def raiseSourceFile(file: SourceFile): SourceFileSource = new SourceFileSource(file)
  implicit def lowerSourceFileSource(source: Source): SourceFile   = source match {
    case sfs: SourceFileSource => sfs.source
    case _                     => NoSourceFile
  }

  def processTabs(s: String): String = s.toCharArray.foldLeft("") {
    case ((res, '\t')) => res + (" " * (8 - res.length % 8))
    case ((res, ch))   => res + ch
  }

  type LineRange     = IndexRange[LineNumber]
  type CharRange     = IndexRange[Char]
  type LineOffset    = Index[LineNumber]
  type CharOffset    = Index[Char]
  type LengthInLines = Length[LineNumber]
  type LengthInChars = Length[Char]
}
