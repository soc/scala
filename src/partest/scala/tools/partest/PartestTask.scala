/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools
package partest

import nest.{ DirectRunner, FileManager }
import scala.util.Properties.setProp
import scala.tools.ant.sabbus.CompilationPathProperty
import java.lang.reflect.Method
import org.apache.tools.ant.Task
import org.apache.tools.ant.types.{ Reference, FileSet}
import org.apache.tools.ant.types.Commandline.Argument

/** An Ant task to execute the Scala test suite (NSC).
 *
 *  This task can take the following parameters as attributes:
 *  - `srcdir`,
 *  - `classpath`,
 *  - `classpathref`,
 *  - `erroronfailed`,
 *  - `scalacopts`,
 *  - `debug`,
 *  - `junitreportdir`.
 *
 *  It also takes the following parameters as nested elements:
 *  - `compilationpath`.
 *
 * @author Philippe Haller
 */
class PartestTask extends Task with CompilationPathProperty {
  type Path = org.apache.tools.ant.types.Path
  def baseDir = project.getBaseDir

  private var kinds: List[String]           = Nil
  private var classpath: Option[Path]       = None
  private var debug                         = false
  private var errorOnFailed: Boolean        = true
  private var jUnitReportDir: Option[File]  = None
  private var runFailed: Boolean            = false
  private var scalacAntArgs: Option[String] = None
  private var srcDir: Option[String]        = None
  private var colors: Int                   = 0

  def setSrcDir(input: String) {
    srcDir = Some(input)
  }

  def setColors(input: String) {
    try colors = input.toInt catch { case _: NumberFormatException => () }
    if (colors > 0)
      sys.props("partest.colors") = colors.toString
  }

  def setClasspath(input: Path) {
    if (classpath.isEmpty)
      classpath = Some(input)
    else
      classpath.get.append(input)
  }

  def createClasspath(): Path = {
    if (classpath.isEmpty) classpath = Some(new Path(getProject()))
    classpath.get.createPath()
  }

  def setClasspathref(input: Reference) {
    createClasspath().setRefid(input)
  }
  def setErrorOnFailed(input: Boolean) {
    errorOnFailed = input
  }

  def setKinds(input: String) {
    kinds = words(input)
  }

  def setScalacOpts(input: String) {
    scalacAntArgs = Some(input)
  }

  def setDebug(input: Boolean) {
    debug = input
  }

  def setJUnitReportDir(input: File) {
    jUnitReportDir = Some(input)
  }

  private def cpurls = this.compilationPath match {
    case Some(cp) => cp.list map (f => SFile(f).toAbsolute.toURL) toList
    case _        => sys.error("Mandatory attribute 'compilationPath' is not set.")
  }

  object antRunner extends DirectRunner {
    object fileManager extends FileManager {
      val buildPrefix            = "build/pack"
      val testRootPath: String   = "test"

      override def latestLibs = cpurls
      override def scalacOpts = super.scalacOpts ++ scalacAntArgs.fold(Seq[String]())(words)
    }

    def reflectiveRunTestsForFiles(kindFiles: Array[File], kind: String): List[TestState] =
      runTestsForFiles(kindFiles.toList, kind)
  }

  override def execute() {
    val jhome = getProject.getProperty("java.home")
    sys.props("partest.javacmd") = s"$jhome/bin/java"
    sys.props("partest.javac_cmd") = s"$jhome/bin/javac"

    if (debug || sys.props.contains("partest.debug")) {
      nest.NestUI.setDebug()
    }
    srcDir foreach (x => setProp("partest.srcdir", x))

    def runSet(kind: String, files: Array[File]): (Int, Int, List[String]) = {
      if (files.isEmpty) (0, 0, List())
      else {
        log(s"Running ${files.length} tests in '$kind' at $now")
        // log(s"Tests: ${files.toList}")
        val results = antRunner.reflectiveRunTestsForFiles(files, kind)
        val (passed, failed) = results partition (_.isOk)
        val numPassed = passed.size
        val numFailed = failed.size
        def failedMessages = failed map (_.longStatus)

        log(s"Completed '$kind' at $now")
        (numPassed, numFailed, failedMessages)
      }
    }

    val _results       = kinds map (k => runSet(k, TestKinds testsFor k map (_.jfile) toArray))
    val allSuccesses   = _results map (_._1) sum
    val allFailures    = _results map (_._2) sum
    val allFailedPaths = _results flatMap (_._3)

    def f = if (errorOnFailed && allFailures > 0) (sys error _) else log(_: String)
    def s = if (allFailures > 1) "s" else ""
    val msg =
      if (allFailures > 0)
        "Test suite finished with %d case%s failing:\n".format(allFailures, s)+
        allFailedPaths.mkString("\n")
      else if (allSuccesses == 0) "There were no tests to run."
      else "Test suite finished with no failures."

    f(msg)
  }
}
