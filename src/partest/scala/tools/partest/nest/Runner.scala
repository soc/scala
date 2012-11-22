package scala.tools.partest
package nest

import java.io._
import scala.tools.nsc.Properties.{ jdkHome, javaHome, propOrElse, propOrEmpty }
import scala.util.Properties.{ envOrElse, isWin }
import scala.tools.nsc.{ Settings, CompilerCommand, Global }
import scala.tools.nsc.io.{ AbstractFile, PlainFile, Path, Directory, File => SFile }
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.util.{ ClassPath, FakePos, ScalaClassLoader, stackTraceString }
import ClassPath.{ join, split }
import scala.collection.{ mutable, immutable }
import scala.tools.nsc.interactive.{ BuildManager, RefinedBuildManager }
import scala.sys.process._
import java.util.concurrent.{ Executors, TimeUnit, TimeoutException }

trait PartestRunSettings {
  def gitPath: Path
  def reportPath: Path
  def logPath: Path

  def testPaths: List[Path]

  def gitDiffOptions: List[String]
  def extraScalacOptions: List[String]
  def extraJavaOptions: List[String]
}

class TestTranscript {
  import NestUI.color._
  private val buf = mutable.ListBuffer[String]()
  private def pass(s: String) = bold(green("% ")) + s
  private def fail(s: String) = bold(red("% ")) + s

  def add(action: String): this.type = { buf += action ; this }
  def append(text: String) { val s = buf.last ; buf.trimEnd(1) ; buf += (s + text) }

  // Colorize prompts according to pass/fail
  def fail: List[String] = buf.toList match {
    case Nil  => Nil
    case xs   => (xs.init map pass) :+ fail(xs.last)
  }
}

class Runner(val testFile: File, fileManager: FileManager) {
  import fileManager._
  // NestUI.debug(s"Created Runner($testFile, $fileManager)")

  // Override to true to have the outcome of this test displayed
  // whether it passes or not; in general only failures are reported,
  // except for a . per passing test to show progress.
  def isEnumeratedTest = false

  private var _lastState: TestState = null
  private var _transcript = new TestTranscript

  def lastState                   = if (_lastState == null) TestState.Uninitialized(testFile) else _lastState
  def setLastState(s: TestState)  = _lastState = s
  def transcript: List[String]    = _transcript.fail ++ logFile.fileLines
  def pushTranscript(msg: String) = _transcript add msg

  val parentFile = testFile.getParentFile
  val kind       = parentFile.getName
  val fileBase   = basename(testFile.getName)
  val logFile    = new File(parentFile, s"$fileBase-$kind.log")
  val outFile    = logFile changeExtension "obj"
  val checkFile  = testFile changeExtension "check"
  val flagsFile  = testFile changeExtension "flags"
  val testIdent  = testFile.testIdent // e.g. pos/t1234

  lazy val outDir = { outFile.mkdirs() ; outFile }

  type RanOneTest = (Boolean, LogContext)

  def showCrashInfo(t: Throwable) {
    System.err.println("Crashed running test $testIdent: " + t)
    if (!isPartestTerse)
      System.err.println(stackTraceString(t))
  }
  protected def crashHandler: PartialFunction[Throwable, TestState] = {
    case t: InterruptedException =>
      genTimeout()
    case t: Throwable =>
      showCrashInfo(t)
      logFile.appendAll(stackTraceString(t))
      genCrash(t)
  }

  def genPass()                   = TestState.Pass(testFile)
  def genFail(reason: String)     = TestState.Fail(testFile, reason, _transcript.fail)
  def genTimeout()                = TestState.Fail(testFile, "timed out", _transcript.fail)
  def genCrash(caught: Throwable) = TestState.Crash(testFile, caught, _transcript.fail)

  def speclib = srcSpecLib.toString  // specialization lib

  // Prepend to a classpath, but without incurring duplicate entries
  def prependTo(classpath: String, path: String): String = {
    val segments = ClassPath split classpath

    if (segments startsWith path) classpath
    else ClassPath.join(path :: segments distinct: _*)
  }

  def prependToJavaClasspath(path: String) {
    val jcp = sys.props.getOrElse("java.class.path", "")
    prependTo(jcp, path) match {
      case `jcp`  =>
      case cp     => sys.props("java.class.path") = cp
    }
  }
  def prependToClasspaths(s: Settings, path: String) {
    prependToJavaClasspath(path)
    val scp = s.classpath.value
    prependTo(scp, path) match {
      case `scp`  =>
      case cp     => s.classpath.value = cp
    }
  }

  private def workerError(msg: String): Unit = System.err.println("Error: " + msg)

  /** This does something about absolute paths and file separator
   *  chars before diffing.
   */
  private def replaceSlashes(s: String): String = {
    val base0 = kind match {
      case "buildmanager" => outDir.getAbsolutePath
      case _              => parentFile.getAbsolutePath
    }
    val base  = canonicalizeSlashes(base0 + '/')
    var regex = """%s\Q%s\E""".format(if (isWin) "(?i)" else "", base)

    s.replaceAll(regex, "")
  }

  def javac(files: List[File]): TestState = {
    // compile using command-line javac compiler
    val args = Seq(
      javacCmd,
      "-d",
      outDir.getAbsolutePath,
      "-classpath",
      stdClassPath
    ) ++ files.map("" + _)

    pushTranscript(args mkString " ")
    val captured = StreamCapture(runCommand(args, logFile))
    if (captured.result) genPass() else {
      logFile appendAll captured.stderr
      genFail("java compilation failed")
    }
  }

  def testPrompt = kind match {
    case "res"  => "nsc> "
    case _      => "% "
  }

  def nextTestAction[T](body: => T)(failFn: PartialFunction[T, TestState]): T = {
    val result = body
    setLastState( if (failFn isDefinedAt result) failFn(result) else genPass() )
    result
  }
  def nextTestActionExpectTrue[T](reason: String, body: => Boolean): Boolean = {
    nextTestAction(body) { case false => genFail(reason) }
  }

  private def assembleTestCommand(outDir: File, logFile: File): List[String] = {
    // check whether there is a ".javaopts" file
    val argsFile  = testFile changeExtension "javaopts"
    val argString = file2String(argsFile)
    if (argString != "")
      NestUI.verbose("Found javaopts file '%s', using options: '%s'".format(argsFile, argString))

    val testFullPath = testFile.getAbsolutePath

    // Note! As this currently functions, JAVA_OPTS must precede argString
    // because when an option is repeated to java only the last one wins.
    // That means until now all the .javaopts files were being ignored because
    // they all attempt to change options which are also defined in
    // partest.java_opts, leading to debug output like:
    //
    // debug: Found javaopts file 'files/shootout/message.scala-2.javaopts', using options: '-Xss32k'
    // debug: java -Xss32k -Xss2m -Xms256M -Xmx1024M -classpath [...]
    val extras = if (isPartestDebug) List("-Dpartest.debug=true") else Nil
    val propertyOptions = List(
      "-Dfile.encoding=UTF-8",
      "-Djava.library.path="+logFile.getParentFile.getAbsolutePath,
      "-Dpartest.output="+outDir.getAbsolutePath,
      "-Dpartest.lib="+latestLib,
      "-Dpartest.reflect="+latestReflect,
      "-Dpartest.cwd="+outDir.getParent,
      "-Dpartest.test-path="+testFullPath,
      "-Dpartest.testname="+fileBase,
      "-Djavacmd="+javaCmd,
      "-Djavaccmd="+javacCmd,
      "-Duser.language=en",
      "-Duser.country=US"
    ) ++ extras

    val javaOptions = stdJavaOpts ++ words(argString)
    val xs = (javaCmd :: javaOptions) ++ ("-classpath" :: stdClassPath :: propertyOptions) ++ Seq(
      "scala.tools.nsc.MainGenericRunner",
      "-usejavacp",
      "Test",
      "jvm"
    )
    xs filterNot (_ == "")
  }

  /** Runs command redirecting standard out and
   *  error out to output file.
   */
  private def runCommand(args: Seq[String], outFile: File): Boolean = {
    blockingSystemExit {
      (Process(args) #> outFile !) == 0
    }
  }

  private def execTest(outDir: File, logFile: File): Boolean = {
    val cmd = assembleTestCommand(outDir, logFile)
    vlog(s"execTest($outDir, $logFile) // cmd = $cmd")

    pushTranscript(cmd.mkString(" \\\n  ") + " > " + logFile.getName)
    nextTestActionExpectTrue("non-zero exit code", runCommand(cmd, logFile)) || {
      _transcript append logFile.fileContents
      false
    }
  }

  override def toString = s"""Test($testIdent, lastState = $lastState)"""

  private def getCheckFilePath(dir: File, suffix: String = "") = {
    def chkFile(s: String) = (Directory(dir) / "%s%s.check".format(fileBase, s)).toFile

    if (chkFile("").isFile || suffix == "") chkFile("")
    else chkFile("-" + suffix)
  }
  private def getCheckFile(dir: File) = Some(getCheckFilePath(dir, kind)) filter (_.canRead)

  def newTestWriters() = {
    val swr = new StringWriter
    val wr  = new PrintWriter(swr, true)
    // diff    = ""

    ((swr, wr))
  }

  def fail(what: Any) = {
    NestUI.verbose("scalac: compilation of "+what+" failed\n")
    false
  }

  def currentDiff = (
    if (checkFile.canRead) compareFiles(logFile, checkFile)
    else compareContents(augmentString(file2String(logFile)).lines.toList, Nil)
  )

  val gitRunner = List("/usr/local/bin/git", "/usr/bin/git") map (f => new java.io.File(f)) find (_.canRead)
  val gitDiffOptions = "--ignore-space-at-eol --no-index " + propOrEmpty("partest.git_diff_options")
    // --color=always --word-diff

  def gitDiff(f1: File, f2: File): Option[String] = {
    try gitRunner map { git =>
      val cmd  = s"$git diff $gitDiffOptions $f1 $f2"
      val diff = Process(cmd).lines_!.drop(4).map(_ + "\n").mkString

      "\n" + diff
    }
    catch { case t: Exception => None }
  }

  def diffIsOk: Boolean = {
    logFile mapInPlace replaceSlashes
    // fileManager.mapFile(logFile, replaceSlashes)

    val diff = currentDiff
    val ok: Boolean = (diff == "") || {
      fileManager.updateCheck && {
        NestUI.verbose("Updating checkfile " + checkFile)
        checkFile writeAll file2String(logFile)
        true
      }
    }
    pushTranscript(s"diff $logFile $checkFile")
    nextTestAction(ok) {
      case false =>
        // Get a word-highlighted diff from git if we can find it
        val bestDiff = if (ok) "" else {
          if (checkFile.canRead)
            gitDiff(logFile, checkFile) getOrElse {
              s"diff $logFile $checkFile\n$diff"
            }
          else diff
        }
        _transcript append bestDiff
        genFail("output differs")
        // TestState.fail("output differs", "output differs",
        // genFail("output differs")
        // TestState.Fail("output differs", bestDiff)
    }
  }

  /** 1. Creates log file and output directory.
   *  2. Runs script function, providing log file and output directory as arguments.
   */
  def runInContext(body: => Boolean): (Boolean, LogContext) = {
    val (swr, wr) = newTestWriters()
    val succeeded = body
    (succeeded, LogContext(logFile, swr, wr))
  }

  def groupedFiles(dir: File): List[List[File]] = {
    val testFiles = dir.listFiles.toList filter (_.isJavaOrScala)

    def isInGroup(f: File, num: Int) = SFile(f).stripExtension endsWith ("_" + num)
    val groups = (0 to 9).toList map (num => testFiles filter (f => isInGroup(f, num)))
    val noGroupSuffix = testFiles filterNot (groups.flatten contains)

    noGroupSuffix :: groups filterNot (_.isEmpty)
  }

  def newCompiler = new DirectCompiler(fileManager)

  def attemptCompile(sources: List[File]): TestState = {
    val state = newCompiler.compile(this, words(file2String(flagsFile)), sources)
    if (!state.isOk)
      _transcript append ("\n" + file2String(logFile))

    state
  }
  abstract class CompileRound {
    def fs: List[File]
    def result: TestState
    def description: String

    def fsString = fs map (_.toString stripPrefix parentFile.toString + "/") mkString " "
    def isOk = result.isOk
    def mkScalacString(): String = {
      val flags = file2String(flagsFile) match {
        case ""   => ""
        case s    => " " + s
      }
      s"""scalac $fsString"""
    }
    override def toString = description + ( if (result.isOk) "" else "\n" + result.status )
  }
  case class OnlyJava(fs: List[File]) extends CompileRound {
    def description = s"""javac $fsString"""
    lazy val result = { pushTranscript(description) ; javac(fs) }
  }
  case class OnlyScala(fs: List[File]) extends CompileRound {
    def description = mkScalacString()
    lazy val result = { pushTranscript(description) ; attemptCompile(fs) }
  }
  case class ScalaAndJava(fs: List[File]) extends CompileRound {
    def description = mkScalacString()
    lazy val result = { pushTranscript(description) ; attemptCompile(fs) }
  }

  def compilationRounds(file: File): List[CompileRound] = {
    val grouped = if (file.isDirectory) groupedFiles(file) else List(List(file))

    (grouped map mixedCompileGroup).flatten
  }
  def mixedCompileGroup(allFiles: List[File]): List[CompileRound] = {
    val (scalaFiles, javaFiles) = allFiles partition (_.isScala)
    val isMixed                 = javaFiles.nonEmpty && scalaFiles.nonEmpty
    val round1                  = if (scalaFiles.isEmpty) None else Some(ScalaAndJava(allFiles))
    val round2                  = if (javaFiles.isEmpty) None else Some(OnlyJava(javaFiles))
    val round3                  = if (!isMixed) None else Some(OnlyScala(scalaFiles))

    List(round1, round2, round3).flatten
  }

  def runNegTest() = runInContext {
    val rounds = compilationRounds(testFile)

    if (rounds forall (x => nextTestActionExpectTrue("compilation failed", x.isOk)))
      nextTestActionExpectTrue("expected compilation failure", false)
    else
      diffIsOk
  }

  def runTestCommon(andAlso: => Boolean): (Boolean, LogContext) = runInContext {
    compilationRounds(testFile).forall(x => nextTestActionExpectTrue("compilation failed", x.isOk)) && andAlso
  }

  def stdClassPathElems = ClassPath.split(stdClassPath)
  def stdClassPath = ClassPath.join(extraClasspath, outDir.getPath, fileManager.universalClasspath)
  def stdJavaOpts: List[String]  = extraJavaOptions ++ fileManager.javaOpts

  def extraClasspath = kind match {
    case "specialized"  => srcSpecLib.toString  // specialization lib
    case _              => ""
  }
  def extraJavaOptions = kind match {
    case "instrumented" => ("-javaagent:"+instrumentationAgentLib) :: Nil
    case _              => Nil
  }

  def runScalacheckTest() = runTestCommon {
    NestUI.verbose(s"compilation of $testFile succeeded\n")

    val outURL    = outDir.toURI.toURL
    val logWriter = new PrintStream(new FileOutputStream(logFile), true)

    // this classloader is test specific: its parent contains library classes and others
    blockingSystemExit {
      Output.withRedirected(logWriter)(newLoader(outURL).run("Test", Nil))
      true
    }

    NestUI.verbose(file2String(logFile))
    // obviously this must be improved upon
    val lines = SFile(logFile).lines map (_.trim) filterNot (_ == "") toBuffer;
    lines.forall(x => !x.startsWith("!")) || {
      NestUI.normal("ScalaCheck test failed. Output:\n")
      lines foreach (x => NestUI.normal(x + "\n"))
      false
    }
  }

  // -    def runSpecializedTest(file: File): (Boolean, LogContext) =
  // -      runTestCommon(file, expectFailure = false)((logFile, outDir) => {
  // -        val dir       = file.getParentFile
  // -
  // -        // adding the instrumented library to the classpath
  // -        ( execTest(outDir, logFile, PathSettings.srcSpecLib.toString) &&
  // -          diffCheck(file, compareOutput(dir, logFile))
  // -        )
  // -      })
  // -

  def run(): TestState = {
    if (kind == "neg" || (kind endsWith "-neg")) runNegTest()
    else kind match {
      case "pos"          => runTestCommon(true)
      // case "specialized"  => runSpecializedTest()
      case "scalacheck"   => runScalacheckTest()
      case "script"       => runScriptTest()
      case _              => runTestCommon(execTest(outDir, logFile) && diffIsOk)
    }

    lastState
  }

  def runScriptTest() = {
    val (swr, wr) = newTestWriters()

    val args = file2String(testFile changeExtension "args")
    val cmdFile = if (isWin) testFile changeExtension "bat" else testFile
    val succeeded = (((cmdFile + " " + args) #> logFile !) == 0) && diffIsOk

    (succeeded, LogContext(logFile, swr, wr))
  }

  def cleanup() {
    if (lastState.isOk)
      logFile.delete()
    if (!isPartestDebug)
      Directory(outDir).deleteRecursively()
  }
}
