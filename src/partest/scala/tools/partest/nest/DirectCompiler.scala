/* NEST (New Scala Test)
 * Copyright 2007-2011 LAMP/EPFL
 * @author Philipp Haller
 */

package scala.tools.partest
package nest

import scala.tools.nsc.{ Global, Settings, CompilerCommand, FatalError }
import scala.tools.nsc.interactive.RangePositions
import scala.tools.nsc.reporters.{ Reporter, ConsoleReporter }
import scala.tools.nsc.util.{ FakePos, stackTraceString }
import scala.tools.nsc.Properties.{ setProp, propOrEmpty }
import scala.reflect.internal.util.Position
import java.io.{ BufferedReader, PrintWriter, FileReader, Writer, FileWriter }

class ExtConsoleReporter(settings: Settings, val writer: PrintWriter) extends ConsoleReporter(settings, Console.in, writer) {
  shortname = true
  // override def error(pos: Position, msg: String): Unit
}

object TestSettings {
  private def errorFn(msg: String) {
    if (isPartestDebug)
      System.err.println(msg)
  }
}
class TestSettings(dir: String) extends Settings(TestSettings.errorFn) {
  outdir.value      = dir
  nowarnings.value  = false
  deprecation.value = true
  encoding.value    = "UTF-8"

  locally {
    outputDirs setSingleOutput dir
    // adding codelib.jar to the classpath
    // codelib provides the possibility to override standard reify
    // this shields the massive amount of reification tests from changes in the API
    // prependToClasspaths(this, codelib)
  }
}

class PartestGlobal(settings: Settings, reporter: Reporter) extends Global(settings, reporter) {
  // override def abort(msg: String): Nothing
  // override def globalError(msg: String): Unit
  // override def supplementErrorMessage(msg: String): String
}
class DirectCompiler(val fileManager: FileManager) {
  def newGlobal(settings: Settings, reporter: Reporter): PartestGlobal =
    if (settings.Yrangepos.value)
      new PartestGlobal(settings, reporter) with RangePositions
    else
      new PartestGlobal(settings, reporter)

  def newGlobal(settings: Settings, logWriter: FileWriter): Global =
    newGlobal(settings, new ExtConsoleReporter(settings, new PrintWriter(logWriter)))

  def compile(runner: Runner, opts0: List[String], sources: List[File]): TestState = {
    import runner._

    val opts         = fileManager.scalacOpts ++ opts0
    val testSettings = new TestSettings(outDir.getPath)
    testSettings.classpath.value = runner.stdClassPath
    val logWriter    = new FileWriter(logFile)
    val command      = new CompilerCommand(opts, testSettings)
    val global       = newGlobal(testSettings, logWriter)
    val reporter     = global.reporter.asInstanceOf[ExtConsoleReporter]
    def errorCount   = reporter.ERROR.count
    val isOk         = testSettings.processArguments(opts, processAll = true)._1

    if (opts.isEmpty)
      reporter.error(null, "bad settings: " + testSettings)
    else if (!isOk)
      reporter.error(null, "bad options: " + opts0.mkString(" "))

    def ids = sources.map(_.testIdent) mkString " "
    vlog(s"% scalac $ids")

    def execCompile() = {
      new global.Run compile sources.map(_.getPath)
      if (!reporter.hasErrors) runner.genPass()
      else {
        reporter.printSummary()
        reporter.writer.close()
        runner.genFail(s"compilation failed with $errorCount errors")
      }
    }

    try     { execCompile() }
    catch   { case t: Throwable => reporter.error(null, t.getMessage) ; runner.genCrash(t) }
    finally { logWriter.close() }
  }
}
