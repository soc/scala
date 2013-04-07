package scala
package reflect

package object naming {
  /** It would be nice if we could move String => TermName/TypeName into the companion
   *  objects of TermName and TypeName, but this is impossible due to SI-7348. Failing
   *  that, something like this is necessary to prevent implicit conflicts from arising
   *  for every method name on Name which also exists in the collections, because
   *  the String => StringOps and String => TermName implicits collide.
   *
   *  We escape the noose by pushing one implicit level deeper with these methods.
   *  It's implicit turtles, all the way down.
   */
  implicit final class StringOpsForNames[T <: Name](val name: NameOfType[T]) extends AnyVal {
    import name._
    def take(n: Int): T      = if (n < length) newName(stringValue take n) else thisName
    def takeRight(n: Int): T = if (n < length) newName(stringValue takeRight n) else thisName
    def drop(n: Int): T      = if (n < length) newName(stringValue drop n) else newName("")
    def dropRight(n: Int): T = if (n < length) newName(stringValue dropRight n) else newName("")

    def slice(from: Int, to: Int) = (
      if (to < from) newName("")
      else mapName(_.substring(0 min from, length max to))
    )
    def stripPrefix(prefix: Name): T   = this stripPrefix prefix.stringValue
    def stripPrefix(prefix: String): T = if (stringValue startsWith prefix) this drop prefix.length else thisName
    def stripSuffix(suffix: Char): T   = if (nonEmpty && endChar == suffix) this dropRight 1 else thisName
    def stripSuffix(suffix: String): T = if (stringValue endsWith suffix) this dropRight suffix.length else thisName
    def stripSuffix(suffix: Name): T   = this stripSuffix suffix.stringValue

    def replace(oldChar: Char, newChar: Char): T = mapName(_.replace(oldChar, newChar))
    def dropWhile(p: Char => Boolean): T = mapName(_ dropWhile p)
    def takeWhile(p: Char => Boolean): T = mapName(_ takeWhile p)
  }
}
