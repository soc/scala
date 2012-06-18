/* NSC -- new Scala compiler
 * Copyright 2006-2011 LAMP/EPFL
 * @author  Lex Spoon
 */

package scala

import scala.tools.nsc._
import java.net.URL
import scala.tools.nsc.io.{ Jar, File }
import scala.tools.nsc.util.{ ClassPath, ScalaClassLoader }
import Properties.{ versionString, copyrightString }
import repl.ILoop
import GenericRunnerCommand._

object JarRunner extends CommonRunner {
  def runJar(settings: GenericRunnerSettings, jarPath: String, arguments: Seq[String]): Either[Throwable, Boolean] = {
    val jar       = new Jar(jarPath)
    val mainClass = jar.mainClass getOrElse sys.error("Cannot find main class for jar: " + jarPath)
    val jarURLs   = ClassPath expandManifestPath jarPath
    val urls      = if (jarURLs.isEmpty) File(jarPath).toURL +: settings.classpathURLs else jarURLs

    if (settings.Ylogcp.value) {
      Console.err.println("Running jar with these URLs as the classpath:")
      urls foreach println
    }

    runAndCatch(urls, mainClass, arguments)
  }
}

/** An object that runs Scala code.  It has three possible
  * sources for the code to run: pre-compiled code, a script file,
  * or interactive entry.
  */
class Repl {
  def errorFn(ex: Throwable): Boolean = {
    ex.printStackTrace()
    false
  }
  def errorFn(str: String): Boolean = {
    Console.err println str
    false
  }

  def process(args: Array[String]): Boolean = {
    val command = new GenericRunnerCommand(args.toList, (x: String) => errorFn(x))
    import command.{ settings, howToRun, thingToRun }
    def sampleCompiler = new Global(settings)   // def so its not created unless needed

    if (!command.ok)                      return errorFn("\n" + command.shortUsageMsg)
    else if (settings.version.value)      return errorFn("Scala code runner %s -- %s".format(versionString, copyrightString))
    else if (command.shouldStopWithInfo)  return errorFn(command getInfoMessage sampleCompiler)

    def runTarget(): Either[Throwable, Boolean] = howToRun match {
      case AsObject =>
        ObjectRunner.runAndCatch(settings.classpathURLs, thingToRun, command.arguments)
      case AsJar    =>
        JarRunner.runJar(settings, thingToRun, command.arguments)
      case Error =>
        Right(false)
      case _  =>
        // We start the repl when no arguments are given.
        Right(new ILoop process settings)
    }

    /** If -e and -i were both given, we want to execute the -e code after the
     *  -i files have been included, so they are read into strings and prepended to
     *  the code given in -e.  The -i option is documented to only make sense
     *  interactively so this is a pretty reasonable assumption.
     *
     *  This all needs a rewrite though.
     */
    runTarget() match {
      case Left(ex) => errorFn(ex)
      case Right(b) => b
    }
  }
}

object Repl extends Repl {
  def main(args: Array[String]) {
    if (!process(args))
      sys.exit(1)
  }
}
