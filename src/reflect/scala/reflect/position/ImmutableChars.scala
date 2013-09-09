package scala
package reflect
package position

import scala.reflect.internal.Chars._
import scala.reflect.io.Streamable

final class ImmutableChars private (underlying: Array[Char]) extends IndexedSeq[Char] {
  def apply(idx: Int)            = underlying(idx)
  def length                     = underlying.length
  def charIndices: Vector[Index] = indices.toVector map Index
  def lineBreaks: Vector[Index]  = charIndices filter isLineBreak
  def lineIndices                = Index(0) +: lineBreaks.map(_.next)
  def lines = (
    for (Vector(start, end) <- lineIndices :+ Index(length) sliding 2) yield
      stringAt(start, end).stripLineEnd
  ).toVector

  def stringAt(start: Index, end: Index): String     = charsAt(start, end).mkString
  def charsAt(range: IndexRange): Array[Char]        = charsAt(range.start, range.end)
  def charsAt(start: Index, end: Index): Array[Char] = underlying.slice(start.value, end.value)
  def viewAt(range: IndexRange): TextView            = new ImmutableTextView(range)
  def charAt(index: Index): Char                     = apply(index.value)

  private def isLast(index: Index) = index.next.value == length
  private def isLineBreak(index: Index) = charAt(index) match {
    case CR => isLast(index) || charAt(index.next) != LF
    case ch => isLineBreakChar(ch)
  }

  private class ImmutableTextView(range: IndexRange) extends TextView {
    def asCharArray = charsAt(range)
    def asLines     = asString.lines.toVector
    def asString    = new String(asCharArray)

    override def toString = asString
  }

  override def toString = s"Array[Char]($length)"
}

object ImmutableChars {
  def apply(chars: Array[Char]): ImmutableChars      = new ImmutableChars(chars)
  def apply(in: java.io.InputStream): ImmutableChars = apply((Streamable slurp in).toCharArray)
  def apply(url: Url): ImmutableChars                = apply(url.openStream())
  def apply(uri: Uri): ImmutableChars                = apply(uri.toURL)
  def apply(file: java.io.File): ImmutableChars      = apply(file.toURI)
}
