/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Alexander Spoon
 */

package scala
package repl

import scala.tools.nsc._
import Predef.{ println => _, _ }
import java.io.{ BufferedReader, FileReader }
import session._
import scala.util.Properties.{ jdkHome, javaVersion, versionString, javaVmName }
import scala.tools.util.{ Javap }
import util.{ ClassPath, Exceptional, stringFromWriter, stringFromStream }
import io.{ File, Directory }
import scala.reflect.NameTransformer._
import util.ScalaClassLoader
import ScalaClassLoader._
import scala.tools.util._
import language.{implicitConversions, existentials}
import java.util.concurrent._

/** The Scala interactive shell.  It provides a read-eval-print loop
 *  around the Interpreter class.
 *  After instantiation, clients should call the main() method.
 *
 *  If no in0 is specified, then input will come from the console, and
 *  the class will attempt to provide input editing feature such as
 *  input history.
 *
 *  @author Moez A. Abdel-Gawad
 *  @author  Lex Spoon
 *  @version 1.2
 */
class ILoop(in0: Option[BufferedReader], protected val out: JPrintWriter) extends LoopCommands {
  def this(in0: BufferedReader, out: JPrintWriter) = this(Some(in0), out)
  def this() = this(None, new JPrintWriter(Console.out, true))

  var in: InteractiveReader = _   // the input stream from which commands come
  var settings: Settings = _
  val queuedLines = new LinkedBlockingQueue[String]
  val synchronousResult = new SynchronousQueue[String]
  // code to be executed only after the interpreter is initialized
  // and the lazy val `global` can be accessed without risk of deadlock.
  val pendingThunks = new ConcurrentLinkedQueue[() => Unit]

  lazy val intp: IMain = new ILoopInterpreter
  lazy val vals = new StdReplVals(this)

  /** Print a welcome message */
  def printWelcome() {
    val welcomeMsg =
     """|Welcome to Scala %s (%s, Java %s).
        |Type in expressions to have them evaluated.
        |Type :help for more information.""" .
    stripMargin.format(versionString, javaVmName, javaVersion)
    echo(welcomeMsg)
    replinfo("[info] started at " + new java.util.Date)
  }

  def initCode = """
import scala.repl._
import $r.replenv._
import treedsl.CODE._
  """.trim

  protected def addThunk(body: => Unit) = pendingThunks add (() => body)
  protected def runThunks() {
    intp.global // force lazy val to completion
    pendingThunks.poll() match {
      case null   => ()
      case thunk  => thunk() ; runThunks()
    }
  }

  // Resuming after ctrl-Z; terminal is hosed.
  SIG.CONT handle {
    if (in != null) {
      repldbg("Caught SIGCONT: resetting terminal.")
      in.reset()
      in.redrawLine()
    }
  }

  // Catching fat-fingered ctrl-Cs.
  SIG.INT handle {
    if (in != null) {
      if (in.currentLine == "") sys.exit(0)
      else {
        echoCommandMessage("\nUse :quit or ctrl-C on an empty line to exit.")
        in.redrawLine()
      }
    }
  }

  /** TODO -
   *  -n normalize
   *  -l label with case class parameter names
   *  -c complete - leave nothing out
   */
  private def typeCommandInternal(expr: String): Result = intp.typeCommand(expr, true)

  override def echoCommandMessage(msg: String) {
    intp.reporter printUntruncatedMessage msg
  }

  def history = in.history

  /** The context class loader at the time this object was created */
  protected val originalClassLoader = Thread.currentThread.getContextClassLoader

  def savingReader[T](body: => T): T = {
    val saved = in
    try body
    finally in = saved
  }

  class ILoopInterpreter extends IMain(settings, out) {
    outer =>

    override lazy val formatting = new Formatting {
      def prompt = ILoop.this.prompt
    }
    override protected def parentClassLoader =
      settings.explicitParentLoader.getOrElse( classOf[ILoop].getClassLoader )
  }

  /** print a friendly help message */
  def helpCommand(line: String): Result = {
    if (line == "") helpSummary()
    else uniqueCommand(line) match {
      case Some(lc) => echo("\n" + lc.longHelp)
      case _        => ambiguousError(line)
    }
  }
  private def helpSummary() = {
    val usageWidth  = commands map (_.usageMsg.length) max
    val formatStr   = "%-" + usageWidth + "s %s %s"

    echo("All commands can be abbreviated, e.g. :he instead of :help.")
    echo("Those marked with a * have more detailed help, e.g. :help imports.\n")

    commands foreach { cmd =>
      val star = if (cmd.hasLongHelp) "*" else " "
      echo(formatStr.format(cmd.usageMsg, star, cmd.help))
    }
  }
  private def ambiguousError(cmd: String): Result = {
    matchingCommands(cmd) match {
      case Nil  => echo(cmd + ": no such command.  Type :help for help.")
      case xs   => echo(cmd + " is ambiguous: did you mean " + xs.map(":" + _.name).mkString(" or ") + "?")
    }
    Result(true, 0)
  }
  private def matchingCommands(cmd: String) = commands filter (_.name startsWith cmd)
  private def uniqueCommand(cmd: String): Option[LoopCommand] = {
    // this lets us add commands willy-nilly and only requires enough command to disambiguate
    matchingCommands(cmd) match {
      case List(x)  => Some(x)
      // exact match OK even if otherwise appears ambiguous
      case xs       => xs find (_.name == cmd)
    }
  }

  // When you know you are most likely breaking into the middle
  // of a line being typed.  This softens the blow.
  protected def echoAndRefresh(msg: String) = {
    echo("\n" + msg)
    in.redrawLine()
  }
  protected def echo(msg: String) = {
    out println msg
    out.flush()
  }
  protected def echoNoNL(msg: String) = {
    out print msg
    out.flush()
  }

  private var currentPrompt = Properties.shellPromptString
  def setPrompt(prompt: String) = currentPrompt = prompt
  /** Prompt to print when awaiting input */
  def prompt = currentPrompt

  import LoopCommand.{ cmd, nullary }

  /** Standard commands **/
  lazy val standardCommands = List(
    cmd("help", "[command]", "print this summary or command-specific help", helpCommand),
    cmd("imports", "[name name ...]", "show import history, identifying sources of names", importsCommand),
    cmd("implicits", "[-v]", "show the implicits in scope", implicitsCommand),
    cmd("javap", "<path|class>", "disassemble a file or class name", javapCommand),
    nullary("paste", "enter paste mode: all input up to ctrl-D compiled together", pasteCommand),
    nullary("quit", "exit the interpreter", () => Result(false, 0)),
    nullary("warnings", "show the suppressed warnings from the most recent line which had any", warningsCommand)
  )

  private def importsCommand(line: String): Result = {
    val tokens    = words(line)
    val handlers  = intp.languageWildcardHandlers ++ intp.importHandlers
    val isVerbose = tokens contains "-v"

    handlers.filterNot(_.importedSymbols.isEmpty).zipWithIndex foreach {
      case (handler, idx) =>
        val (types, terms) = handler.importedSymbols partition (_.name.isTypeName)
        val imps           = handler.implicitSymbols
        val found          = tokens filter (handler exposesName _)
        val typeMsg        = if (types.isEmpty) "" else types.size + " types"
        val termMsg        = if (terms.isEmpty) "" else terms.size + " terms"
        val implicitMsg    = if (imps.isEmpty) "" else imps.size + " are implicit"
        val foundMsg       = if (found.isEmpty) "" else found.mkString(" // imports: ", ", ", "")
        val statsMsg       = List(typeMsg, termMsg, implicitMsg) filterNot (_ == "") mkString ("(", ", ", ")")

        intp.reporter.printMessage("%2d) %-30s %s%s".format(
          idx + 1,
          handler.importString,
          statsMsg,
          foundMsg
        ))
    }
  }

  private def implicitsCommand(line: String): Result = {
    // If an argument is given, only show a source with that
    // in its name somewhere.
    val args = line split "\\s+"
    val filter: Global#Symbol => Boolean = (
      if (args contains "-v") _ => true
      else if (line == "") s => s.fullName.toString != "scala.Predef"
      else s => args exists (s.name.toString contains _)
    )

    if (intp.implicitsCommand(filter)) ()
    else "No implicits have been imported other than those in Predef."
  }

  private def findToolsJar() = {
    val jdkPath = Directory(jdkHome)
    val jar     = jdkPath / "lib" / "tools.jar" toFile;

    if (jar isFile)
      Some(jar)
    else if (jdkPath.isDirectory)
      jdkPath.deepFiles find (_.name == "tools.jar")
    else None
  }
  private def addToolsJarToLoader() = {
    val cl = findToolsJar match {
      case Some(tools) => ScalaClassLoader.fromURLs(Seq(tools.toURL), intp.classLoader)
      case _           => intp.classLoader
    }
    if (Javap.isAvailable(cl)) {
      repldbg(":javap available.")
      cl
    }
    else {
      repldbg(":javap unavailable: no tools.jar at " + jdkHome)
      intp.classLoader
    }
  }

  protected def newJavap() = new JavapClass(addToolsJarToLoader(), new IMain.ReplStrippingWriter(intp)) {
    override def tryClass(path: String): Array[Byte] = {
      val hd :: rest = path split '.' toList;
      // If there are dots in the name, the first segment is the
      // key to finding it.
      if (rest.nonEmpty) {
        intp optFlatName hd match {
          case Some(flat) =>
            val clazz = flat :: rest mkString NAME_JOIN_STRING
            val bytes = super.tryClass(clazz)
            if (bytes.nonEmpty) bytes
            else super.tryClass(clazz + MODULE_SUFFIX_STRING)
          case _          => super.tryClass(path)
        }
      }
      else {
        // Look for Foo first, then Foo$, but if Foo$ is given explicitly,
        // we have to drop the $ to find object Foo, then tack it back onto
        // the end of the flattened name.
        def className  = intp flatName path
        def moduleName = (intp flatName path.stripSuffix(MODULE_SUFFIX_STRING)) + MODULE_SUFFIX_STRING

        val bytes = super.tryClass(className)
        if (bytes.nonEmpty) bytes
        else super.tryClass(moduleName)
      }
    }
  }
  private lazy val javap = substituteAndLog[Javap]("javap", NoJavap)(newJavap())

  private def warningsCommand(): Result = {
    intp.lastWarnings foreach { case (pos, msg) => intp.reporter.warning(pos, msg) }
  }

  private def javapCommand(line: String): Result = {
    if (javap == null)
      ":javap unavailable, no tools.jar at %s.  Set JDK_HOME.".format(jdkHome)
    else if (javaVersion startsWith "1.7")
      ":javap not yet working with java 1.7"
    else if (line == "")
      ":javap [-lcsvp] [path1 path2 ...]"
    else
      javap(words(line)) foreach { res =>
        if (res.isError) return "Failed: " + res.value
        else res.show()
      }
  }

  /** Available commands */
  def commands: List[LoopCommand] = standardCommands

  private object readerThread extends Thread {
    this setPriority Thread.MAX_PRIORITY
    this setDaemon true

    private var running = true
    def stillRunning = running
    def stopRunning() = running = false

    override def run() {
      while (stillRunning) {
        out.flush()
        val line = in readLine prompt

        if (line == null)
          stopRunning()
        else {
          queuedLines put line
          synchronousResult.take()
        }
      }
    }
  }

  /** The main read-eval-print loop for the repl.  It calls
   *  command() for each line of input, and stops when
   *  command() returns false.
   */
  def loop() {
    while (readerThread.stillRunning) {
      runThunks()
      val line = queuedLines.take()

      command(line) match {
        case Result(false, _) =>
          readerThread.stopRunning()
        case Result(true, n)  =>
          if ((history ne NoHistory) && (n > 1)) {
            val coalesced = history.removeRange(history.size - n, n) mkString "\n"
            history add coalesced
          }
          synchronousResult put "ok"
      }
    }
    echo("Exiting repl loop.")
  }

  /** Run one command submitted by the user.  Two values are returned:
    * (1) whether to keep running, (2) the line to record for replay,
    * if any. */
  def command(line: String): Result = {
    if (line startsWith ":") {
      val cmd = line.tail takeWhile (x => !x.isWhitespace)
      uniqueCommand(cmd) match {
        case Some(lc) => lc(line.tail stripPrefix cmd dropWhile (_.isWhitespace))
        case _        => ambiguousError(cmd)
      }
    }
    else if (intp.global == null) Result(false, 0)  // Notice failure to create compiler
    else Result(true, interpretStartingWith(line, 1))
  }

  private def readWhile(cond: String => Boolean) = {
    Iterator continually in.readLine("") takeWhile (x => x != null && cond(x))
  }

  def pasteCommand(): Result = {
    echo("// Entering paste mode (ctrl-D to finish)\n")
    val code = readWhile(_ => true) mkString "\n"
    echo("\n// Exiting paste mode, now interpreting.\n")
    intp interpret code
    ()
  }

  private object paste extends Pasted {
    val ContinueString = "     | "
    val PromptString   = "scala> "

    def interpret(line: String): Unit = {
      echo(line.trim)
      intp interpret line
      echo("")
    }

    def transcript(start: String) = {
      echo("\n// Detected repl transcript paste: ctrl-D to finish.\n")
      apply(Iterator(start) ++ readWhile(_.trim != PromptString.trim))
    }
  }
  import paste.{ ContinueString, PromptString }

  /** Interpret expressions starting with the first line.
    * Read lines until a complete compilation unit is available
    * or until a syntax error has been seen.  If a full unit is
    * read, go ahead and interpret it.  Return the number of
    * lines read.
    */
  def interpretStartingWith(code: String, linesRead: Int): Int = {
    // signal completion non-completion input has been received
    in.completion.resetVerbosity()

    /** Here we place ourselves between the user and the interpreter and examine
     *  the input they are ostensibly submitting.  We intervene in several cases:
     *
     *  1) If the line starts with "scala> " it is assumed to be an interpreter paste.
     *  2) If the line starts with "." (but not ".." or "./") it is treated as an invocation
     *     on the previous result.
     *  3) If the Completion object's execute returns Some(_), we inject that value
     *     and avoid the interpreter, as it's likely not valid scala code.
     */
    if (code == "") linesRead
    else if (!paste.running && code.trim.startsWith(PromptString)) {
      paste.transcript(code)
      linesRead
    }
    else if (Completion.looksLikeInvocation(code) && intp.mostRecentVar != "") {
      interpretStartingWith(intp.mostRecentVar + code, linesRead)
    }
    else if (code.trim startsWith "//") {
      // line comment, do nothing
      linesRead
    }
    else intp.interpret(code) match {
      case IR.Error       => linesRead
      case IR.Success     => linesRead
      case IR.Incomplete  =>
        if (in.interactive && code.endsWith("\n\n")) {
          echo("You typed two blank lines.  Starting a new command.")
          linesRead
        }
        else in.readLine(ContinueString) match {
          case null =>
            // we know compilation is going to fail since we're at EOF and the
            // parser thinks the input is still incomplete, but since this is
            // a file being read non-interactively we want to fail.  So we send
            // it straight to the compiler for the nice error message.
            intp.compileString(code)
            linesRead

          case line => interpretStartingWith(code + "\n" + line, linesRead + 1)
        }
    }
  }

  /** Tries to create a JLineReader, falling back to SimpleReader:
   *  unless settings or properties are such that it should start
   *  with SimpleReader.
   */
  def chooseReader(settings: Settings): InteractiveReader = {
    try {
      if (settings.Xnojline.value) SimpleReader()
      else new JLineReader(new JLineCompletion(intp))
    }
    catch {
      case ex @ (_: Exception | _: NoClassDefFoundError) =>
        echo("Failed to created JLineReader: " + ex + "\nFalling back to SimpleReader.")
        SimpleReader()
    }
  }
  def process(settings: Settings): Boolean = savingContextLoader {
    this.settings = settings
    // sets in to some kind of reader depending on environmental cues
    in = in0 match {
      case Some(reader) => SimpleReader(reader, out, true)
      case None         =>
        // some post-initialization
        chooseReader(settings) match {
          case x: JLineReader => addThunk(x.consoleReader.postInit) ; x
          case x              => x
        }
    }

    printWelcome()
    readerThread.start()

    // addThunk({
    //   import scala.tools.nsc.io._
    //   import Properties.userHome
    //   import compat.Platform.EOL
    //   val autorun = replProps.replAutorunCode.option flatMap (f => io.File(f).safeSlurp())
    //   if (autorun.isDefined) intp.quietRun(autorun.get)
    // })

    // it is broken on startup; go ahead and exit
    if (intp.reporter.hasErrors)
      return false

    // Bind intp somewhere out of the regular namespace where
    // we can get at it in generated code.
    addThunk(intp.quietly {
      intp.setContextClassLoader
      intp.bind("$intp" -> intp)
      // First we create the ReplVals instance and bind it to $r
      intp.bind("$r" -> vals)
      // Then we import everything from $r, via its true path.
      // Later imports rely on the repl's name resolution to find $r.
      intp interpret ("import " + intp.pathToTerm("$r") + "._")
      // And whatever else there is to do.
      intp interpret initCode
    })

    try loop() finally intp.close()
    true
  }

  /** process command-line arguments and do as they request */
  def process(args: Array[String]): Boolean = {
    val command = new CommandLine(args.toList, echo)
    def neededHelp(): String =
      (if (command.settings.help.value) command.usageMsg + "\n" else "") +
      (if (command.settings.Xhelp.value) command.xusageMsg + "\n" else "")

    // if they asked for no help and command is valid, we call the real main
    neededHelp() match {
      case ""     => command.ok && process(command.settings)
      case help   => echoNoNL(help) ; true
    }
  }
}

object ILoop {
  implicit def loopToInterpreter(repl: ILoop): IMain = repl.intp

  // Designed primarily for use by test code: take a String with a
  // bunch of code, and prints out a transcript of what it would look
  // like if you'd just typed it into the repl.
  def runForTranscript(code: String, settings: Settings): String = {
    import java.io.{ BufferedReader, StringReader, OutputStreamWriter }

    stringFromStream { ostream =>
      Console.withOut(ostream) {
        val output = new JPrintWriter(new OutputStreamWriter(ostream), true) {
          override def write(str: String) = {
            // completely skip continuation lines
            if (str forall (ch => ch.isWhitespace || ch == '|')) ()
            // print a newline on empty scala prompts
            else if ((str contains '\n') && (str.trim == "scala> ")) super.write("\n")
            else super.write(str)
          }
        }
        val input = new BufferedReader(new StringReader(code)) {
          override def readLine(): String = {
            val s = super.readLine()
            // helping out by printing the line being interpreted.
            if (s != null)
              output.println(s)
            s
          }
        }
        val repl = new ILoop(input, output)
        if (settings.classpath.isDefault)
          settings.classpath.value = sys.props("java.class.path")

        repl process settings
      }
    }
  }

  /** Creates an interpreter loop with default settings and feeds
   *  the given code to it as input.
   */
  def run(code: String, sets: Settings = new Settings): String = {
    import java.io.{ BufferedReader, StringReader, OutputStreamWriter }

    stringFromStream { ostream =>
      Console.withOut(ostream) {
        val input    = new BufferedReader(new StringReader(code))
        val output   = new JPrintWriter(new OutputStreamWriter(ostream), true)
        val repl     = new ILoop(input, output)

        if (sets.classpath.isDefault)
          sets.classpath.value = sys.props("java.class.path")

        repl process sets
      }
    }
  }
  def run(lines: List[String]): String = run(lines map (_ + "\n") mkString)
}
