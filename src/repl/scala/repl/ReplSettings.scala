/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Alexander Spoon
 */

package scala.repl

/** Settings for the interpreter
 *
 * @version 1.0
 * @author Lex Spoon, 2007/3/24
 **/

trait ReplSettings {
  def compilerSettings: scala.tools.nsc.Settings

  /** A list of paths where :load should look */
  var loadPath = List(".")

  /** Set this to true to see repl machinery under -Yrich-exceptions.
   */
  var showInternalStackTraces = false

  /** The maximum length of toString to use when printing the result
   *  of an evaluation.  0 means no maximum.  If a printout requires
   *  more than this number of characters, then the printout is
   *  truncated.
   */
  var maxPrintString = replProps.maxPrintString.option.getOrElse(800)

  /** The maximum number of completion candidates to print for tab
   *  completion without requiring confirmation.
   */
  var maxAutoprintCompletion = 250

  /** String unwrapping can be disabled if it is causing issues.
   *  Settings this to false means you will see Strings like "$iw.$iw.".
   */
  var unwrapStrings = true

  def deprecation = compilerSettings.deprecation.value
  def deprecation_=(x: Boolean) = {
    val saved = deprecation
    compilerSettings.deprecation.value = x
    if (!saved && x) println("Enabled -deprecation output.")
    else if (saved && !x) println("Disabled -deprecation output.")
  }
}
