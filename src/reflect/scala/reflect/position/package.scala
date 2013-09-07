package scala
package reflect

package object position {
  private[position] implicit lazy val implicitConversions = scala.language.implicitConversions
  private[position] def illegalArg(msg: Any): Nothing = throw new IllegalArgumentException("" + msg)

  /** Return a same-length string as the basis with all characters
   *  turned into spaces, except tabs which remain tabs.
   */
  private[position] def mapToWhitespace(basis: String): String =
    basis map (ch => if (ch == '\t') ch else ' ') mkString ""
}