/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Lex Spoon
 */

package scala.repl

import scala.tools.nsc._

/** A command line for the interpreter.
 */
class CommandLine(arguments: List[String], error: String => Unit) extends CompilerCommand(arguments, error) {
  override def cmdName = "scala"
  override lazy val fileEndings = List(".scalaint")
}
