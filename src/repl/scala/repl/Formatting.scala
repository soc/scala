/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.repl

import scala.tools.nsc.util.stringFromWriter

trait Formatting {
  def prompt: String

  def spaces(code: String): String = {
    /** Heuristic to avoid indenting and thereby corrupting """-strings and XML literals. */
    val tokens = List("\"\"\"", "</", "/>")
    val noIndent = (code contains "\n") && (tokens exists code.contains)

    if (noIndent) ""
    else prompt drop 1 map (_ => ' ')
  }
 
  /** Indent a few spaces so compiler error messages read better.
   */
  def indentCode(code: String) = code.lines.map("  " + _).mkString("\n")
}
