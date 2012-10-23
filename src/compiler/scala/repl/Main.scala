package scala.repl

import Predef.{ println => _, _ }
import java.io.{ BufferedReader, FileReader }
import java.util.concurrent.locks.ReentrantLock
import scala.sys.process.Process
import session._
import scala.tools.util.{ Signallable, Javap }
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.concurrent.ops
import util.{ ClassPath, Exceptional, stringFromWriter, stringFromStream }
import interpreter._
import io.{ File, Sources }
import scala.reflect.NameTransformer._

package api {
  trait Repl {
    trait Result { }
    trait SessionResult extends Result
    trait LineResult extends Result
    trait CompletionResult extends Result

    def completion(line: String): CompletionResult
    def incompleteLines: Seq[String]
    def fail(line: String): LineResult
    def interpret(line: String): LineResult
    def isCommand(line: String): Boolean
    def isInitialized: Boolean
    def isParseable(line: String): Boolean
    def isPaste(line: String): Boolean
    def prompt: String
    def dispatch(line: String): Result
    def handleResult[T](code: String, value: T): Unit
    def handleError(code: String, msg: String): Unit
  }
}

trait ReplCommon extends api.Repl {
  case object NoCompletions extends CompletionResult
  case class Completions(values: List[String]) extends CompletionResult

  case object RebootRepl extends LineResult
  case object QuitRepl extends LineResult
  case object ReplNotReady extends LineResult
  case class LineError(message: String) extends LineResult
  case class QuitRepl(exitCode: Int) extends LineResult
  case class ValueOfExpr[T](value: T) extends LineResult
  case class LineIncomplete extends LineResult

  val ReadNoLines = ReadLines(Nil)
  case class ReadLines(lines: Seq[String]) extends Result {
    def :+(line: String): ReadLines = ReadLines(lines :+ line)
    override def toString = lines mkString "\n"
  }
  case object ReadEOF extends Result
  case object ReadLine extends Result

  def prompt = "scala>"
  def isCommand(line: String) = line startsWith ":"
  def handleResult[T](code: String, value: T) {
  }
  def handleError(code: String, msg: String) {

  }
  def handleEOF()

  private var lines: Seq[String] = Nil


  def readOneLine(): Result = in.readLine(prompt) match {
    case null => ReadEOF
    case line => lines = lines :+ line ; ReadLine
  }
  def incompleteLines = lines
  def clearLines() = lines = Nil
}

class UninitializedRepl(in: InputStream, out: OutputStream) {
  def completion(line: String) = NoCompletions
  def fail(line: String) = ()
  def interpret(line: String) = ReplNotReady
  def isInitialized = false
  def isParseable(line: String) = false
  def isPaste(line: String) = false

  def dispatch(code: String): Result = {
    if (isCommand(code)) command(code)
    else completion(code)
  }
}

abstract class Repl(in: InputStream, out: OutputStream) {
  val intp: IMain

  def completion(line: String): CompletionResult
  def fail(line: String) = ()
  def isInitialized = true
  def isParseable(line: String) = intp isParseable code
  def isPaste(line: String) = line startsWith prompt

  def dispatch(code: String): Result = {
    if (code eq null) ReadEOF
    else if (isCommand(code)) command(code)
    // else if (!isInitialized) completion(code)
    // else if (line startsWith ".") completionOnLastResult(line)
    // else if (isPaste(line)) paste(line)
    else if (isParseable(code)) interpret(code)
    else completion(code)
  }

  @tailrec final def processingLoop(): Result = {
    readOneLine() match {
      case ReadEOF  => System.exit(1)
      case ReadLine => ()
    }
    val code   = incompleteLines mkString "\n"
    val result = dispatch(code)
    if (result == LineIncomplete)
      processingLoop()
    else {
      result match {
        case ValueOfExpr(value) => handleResult(code, value)
        case QuitRepl(code)     => System.exit(code)
        case LineError(msg)     => handleError(code, msg)
        case _                  => handleError(code, "???")
      }
      clearLines()
      processingLoop()
    }
  }

  private def reallyInterpret(code: String): Result = {
    val reallyResult = intp.interpret(code)
    reallyResult match {
      case IR.Error       => LineError
      case IR.Success     => ReplValue(???)
      case IR.Incomplete  => ReplIncomplete(code)
    }
  }

  def command(line: String): Result = {
    val cmd = line.tail takeWhile (x => !x.isWhitespace)
    uniqueCommand(cmd) match {
      case Some(lc) => lc(line.tail stripPrefix cmd dropWhile (_.isWhitespace))
      case _        => ambiguousError(cmd)
    }
  }

  /** Standard commands **/
  lazy val standardCommands = List(
    cmd("help", "[command]", "print this summary or command-specific help", helpCommand),
    nullary("keybindings", "show how ctrl-[A-Z] and other keys are bound", keybindingsCommand),
    nullary("quit", "exit the interpreter", () => Result(false, None))
  )
}
