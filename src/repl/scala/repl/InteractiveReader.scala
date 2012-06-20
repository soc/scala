/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.repl

/** Reads lines from an input stream */
trait InteractiveReader {
  val interactive: Boolean

  def init(): Unit
  def reset(): Unit

  def history: session.History
  def completion: Completion
  def eraseLine(): Unit
  def redrawLine(): Unit
  def currentLine: String

  def readYesOrNo(prompt: String, alt: => Boolean): Boolean = readOneKey(prompt) match {
    case 'y'  => true
    case 'n'  => false
    case _    => alt
  }
  def readAssumingNo(prompt: String)  = readYesOrNo(prompt, false)
  def readAssumingYes(prompt: String) = readYesOrNo(prompt, true)

  protected def readOneLine(prompt: String): String
  protected def readOneKey(prompt: String): Int

  def readLine(prompt: String): String = readOneLine(prompt)
}
