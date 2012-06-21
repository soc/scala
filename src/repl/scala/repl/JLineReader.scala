/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Stepan Koltsov
 */

package scala.repl

import scala.tools.nsc._
import jline.console.{ KeyMap, ConsoleReader }
import jline.console.completer._
import session._
import Completion._

/**
 *  Reads from the console using JLine.
 */
class JLineReader(_completion: => Completion) extends InteractiveReader {
  val interactive = true
  val consoleReader = new JLineConsoleReader()

  lazy val completion = _completion
  lazy val history: JLineHistory = JLineHistory()

  private def term = consoleReader.getTerminal()
  def reset() = term.reset()
  def init()  = term.init()

  def scalaToJline(tc: ScalaCompleter): Completer = new Completer {
    def complete(_buf: String, cursor: Int, candidates: JList[CharSequence]): Int = {
      val buf   = if (_buf == null) "" else _buf
      val Candidates(newCursor, newCandidates) = tc.complete(buf, cursor)
      newCandidates foreach (candidates add _)
      newCursor
    }
  }

  class JLineConsoleReader extends ConsoleReader with ConsoleReaderHelper {
    // public static final String VI_MOVE        = "vi-move";
    // public static final String VI_INSERT      = "vi-insert";
    // public static final String EMACS          = "emacs";
    // public static final String EMACS_STANDARD = "emacs-standard";
    // public static final String EMACS_CTLX     = "emacs-ctlx";
    // public static final String EMACS_META     = "emacs-meta";

    def enterViInsertMode() = setKeyMap(KeyMap.VI_INSERT)
    def enterViMoveMode()   = setKeyMap(KeyMap.VI_MOVE)
    def enterEmacsMode()    = setKeyMap(KeyMap.EMACS)

    // working around protected/trait/java insufficiencies.
    def goBack(num: Int): Unit = back(num)
    def readOneKey(prompt: String) = {
      this.print(prompt)
      this.flush()
      this.readCharacter()
    }
    def eraseLine() = consoleReader.resetPromptLine("", "", 0)
    def redrawLineAndFlush(): Unit = { flush() ; drawLine() ; flush() }
    // override def readLine(prompt: String): String

    // A hook for running code after the repl is done initializing.
    lazy val postInit: Unit = {
      this setBellEnabled false
      this setExpandEvents false
      if ((history: History) ne NoHistory)
        this setHistory history

      if (completion ne NoCompletion) {
        val argCompletor: ArgumentCompleter =
          new ArgumentCompleter(new JLineDelimiter, scalaToJline(completion.completer()))
        argCompletor setStrict false

        this addCompleter argCompletor
        this setAutoprintThreshold 400 // max completion candidates without warning
      }
    }
  }

  def currentLine = consoleReader.getCursorBuffer.buffer.toString
  def redrawLine() = consoleReader.redrawLineAndFlush()
  def eraseLine() = consoleReader.eraseLine()
  // Alternate implementation, not sure if/when I need this.
  // def eraseLine() = while (consoleReader.delete()) { }
  def readOneLine(prompt: String) = consoleReader readLine prompt
  def readOneKey(prompt: String)  = consoleReader readOneKey prompt
}
