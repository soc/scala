/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

import java.io.{BufferedReader, InputStream, InputStreamReader,
                IOException, OutputStream, PrintStream, Reader}
import java.text.MessageFormat
import scala.collection.{ mutable, immutable }

/** Implements functionality for
 *  printing Scala values on the terminal as well as reading specific values.
 *  Also defines constants for marking up text on ANSI terminals.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 03/09/2003
 */
object Console {

  /** Foreground color for ANSI black */
  final val BLACK      = "\033[30m"
  /** Foreground color for ANSI red */
  final val RED        = "\033[31m"
  /** Foreground color for ANSI green */
  final val GREEN      = "\033[32m"
  /** Foreground color for ANSI yellow */
  final val YELLOW     = "\033[33m"
  /** Foreground color for ANSI blue */
  final val BLUE       = "\033[34m"
  /** Foreground color for ANSI magenta */
  final val MAGENTA    = "\033[35m"
  /** Foreground color for ANSI cyan */
  final val CYAN       = "\033[36m"
  /** Foreground color for ANSI white */
  final val WHITE      = "\033[37m"

  /** Background color for ANSI black */
  final val BLACK_B    = "\033[40m"
  /** Background color for ANSI red */
  final val RED_B      = "\033[41m"
  /** Background color for ANSI green */
  final val GREEN_B    = "\033[42m"
  /** Background color for ANSI yellow */
  final val YELLOW_B   = "\033[43m"
  /** Background color for ANSI blue */
  final val BLUE_B     = "\033[44m"
  /** Background color for ANSI magenta */
  final val MAGENTA_B  = "\033[45m"
  /** Background color for ANSI cyan */
  final val CYAN_B     = "\033[46m"
  /** Background color for ANSI white */
  final val WHITE_B    = "\033[47m"

  /** Reset ANSI styles */
  final val RESET      = "\033[0m"
  /** ANSI bold */
  final val BOLD       = "\033[1m"
  /** ANSI underlines */
  final val UNDERLINED = "\033[4m"
  /** ANSI blink */
  final val BLINK      = "\033[5m"
  /** ANSI reversed */
  final val REVERSED   = "\033[7m"
  /** ANSI invisible */
  final val INVISIBLE  = "\033[8m"

  private var globalOut: PrintStream   = java.lang.System.out
  private var globalErr: PrintStream   = java.lang.System.err
  private var globalIn: BufferedReader = new BufferedReader(new InputStreamReader(java.lang.System.in))

  private val threadOuts = mutable.Map[Thread, PrintStream]() withDefault (_ => globalOut)
  private val threadErrs = mutable.Map[Thread, PrintStream]() withDefault (_ => globalErr)
  private val threadIns  = mutable.Map[Thread, BufferedReader]() withDefault (_ => globalIn)

  /** The default output, can be overridden by `setOut` */
  def out = threadOuts(Thread.currentThread)
  /** The default error, can be overridden by `setErr` */
  def err = threadErrs(Thread.currentThread)
  /** The default input, can be overridden by `setIn` */
  def in = threadIns(Thread.currentThread)

  /** Sets the default output stream.
   *
   *  @param out the new output stream.
   */
  def setOut(out: PrintStream) {
    threadOuts.clear()
    globalOut = out
  }
  def setOut(out: OutputStream) {
    setOut(new PrintStream(out))
  }

  /** Sets the default output stream for the duration
   *  of execution of one thunk.
   *
   *  @example {{{
   *  withOut(Console.err) { println("This goes to default _error_") }
   *  }}}
   *
   *  @param out the new output stream.
   *  @param thunk the code to execute with
   *               the new output stream active
   *  @return the results of `thunk`
   *  @see `withOut[T](out:OutputStream)(thunk: => T)`
   */
  def withOut[T](out: PrintStream)(thunk: => T): T = {
    val t = Thread.currentThread
    val saved = threadOuts get t
    threadOuts(t) = out
    try thunk
    finally saved foreach (threadOuts(t) = _)
  }

  /** Sets the default output stream for the duration
   *  of execution of one thunk.
   *
   *  @param out the new output stream.
   *  @param thunk the code to execute with
   *               the new output stream active
   *  @return the results of `thunk`
   *  @see `withOut[T](out:PrintStream)(thunk: => T)`
   */
  def withOut[T](out: OutputStream)(thunk: =>T): T =
    withOut(new PrintStream(out))(thunk)

  /** Sets the default error stream.
   *
   *  @param err the new error stream.
   */
  def setErr(err: PrintStream) {
    threadErrs.clear()
    globalErr = err
  }
  def setErr(err: OutputStream) {
    setErr(new PrintStream(err))
  }

  /** Set the default error stream for the duration
   *  of execution of one thunk.
   *  @example {{{
   *  withErr(Console.out) { println("This goes to default _out_") }
   *  }}}
   *
   *  @param err the new error stream.
   *  @param thunk the code to execute with
   *               the new error stream active
   *  @return the results of `thunk`
   *  @see `withErr[T](err:OutputStream)(thunk: =>T)`
   */
  def withErr[T](err: PrintStream)(thunk: => T): T = {
    val t = Thread.currentThread
    val saved = threadErrs get t
    threadErrs(t) = err
    try thunk
    finally saved foreach (threadErrs(t) = _)
  }
  def withErr[T](err: OutputStream)(thunk: =>T): T =
    withErr(new PrintStream(err))(thunk)

  /** Sets the default input stream.
   *
   *  @param reader specifies the new input stream.
   */
  def setIn(reader: Reader) {
    threadIns.clear()
    globalIn = new BufferedReader(reader)
  }
  def setIn(in: InputStream) {
    setIn(new InputStreamReader(in))
  }

  /** Sets the default input stream for the duration
   *  of execution of one thunk.
   *
   *  @example {{{
   *  val someFile:Reader = openFile("file.txt")
   *  withIn(someFile) {
   *    // Reads a line from file.txt instead of default input
   *    println(readLine)
   *  }
   *  }}}
   *
   *  @param thunk the code to execute with
   *               the new input stream active
   *
   * @return the results of `thunk`
   * @see `withIn[T](in:InputStream)(thunk: =>T)`
   */
  def withIn[T](reader: Reader)(thunk: => T): T = {
    val t = Thread.currentThread
    val saved = threadIns get t
    threadIns(t) = new BufferedReader(reader)
    try thunk
    finally saved foreach (threadIns(t) = _)
  }
  def withIn[T](in: InputStream)(thunk: =>T): T =
    withIn(new InputStreamReader(in))(thunk)

  /** Read a full line from the default input.  Returns `null` if the end of the
   * input stream has been reached.
   *
   * @return the string read from the terminal or null if the end of stream was reached.
   */
  def readLine(): String = in.readLine()

  /** Prints an object to `out` using its `toString` method.
   *
   *  @param obj the object to print; may be null.
   */
  def print(obj: Any) { out print "" + obj }

  /** Flushes the output stream. This function is required when partial
   *  output (i.e. output not terminated by a newline character) has
   *  to be made visible on the terminal.
   */
  def flush() { out.flush() }

  /** Prints out an object to the default output, followed by a newline character.
   *
   *  @param x the object to print.
   */
  def println(x: Any) { out.println(x) }
}
