/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Philipp Haller
 */

package scala.tools.partest
package nest

import scala.tools.nsc.Properties.{ setProp, propOrEmpty }
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.io
import io.Path
import java.net.URLClassLoader

/* This class is used to load an instance of DirectRunner using
 * a custom class loader.
 * The purpose is to "auto-detect" a good classpath for the
 * rest of the classes (Worker, CompileManager etc.), so that
 * the main NestRunner can be started merely by putting its
 * class on the classpath (ideally).
 */
class ReflectiveRunner {
  // TODO: we might also use fileManager.CLASSPATH
  // to use the same classes as used by `scala` that
  // was used to start the runner.
  val sepRunnerClassName = "scala.tools.partest.nest.ConsoleRunner"

  private def searchPath(option: String, as: List[String]): Option[String] = as match {
    case `option` :: r :: _ => Some(r)
    case _ :: rest          => searchPath(option, rest)
    case Nil                => None
  }

  def main(args: String) {
    val argList     = (args.split("\\s")).toList
    val buildPath   = searchPath("--buildpath", argList)
    val classPath   = searchPath("--classpath", argList)
    val fileManager = new ConsoleFileManager(buildPath getOrElse (classPath getOrElse "build/pack"))
    val sepLoader   = fileManager.newLoader()

    // val sepUrls      = PathSettings.srcCodeLib.toURL +: fileManager.latestLibs.toArray
    // val sepLoader    = new URLClassLoader(sepUrls, null)
    // val newClasspath = ClassPath.fromURLs(sepUrls: _*)

    // setProp("java.class.path", newClasspath)
    // setProp("scala.home", "")
    // if (isPartestDebug)
    //   for (prop <- List("java.class.path", "sun.boot.class.path", "java.ext.dirs"))
    //     println(prop + ": " + propOrEmpty(prop))

    val sepRunnerClass  = sepLoader loadClass sepRunnerClassName
    val sepRunner       = sepRunnerClass.newInstance()
    val sepMainMethod   = sepRunnerClass.getMethod("main", Array(classOf[String]): _*)
    val cargs: Array[AnyRef] = Array(args)

    blockingSystemExit({ sepMainMethod.invoke(sepRunner, cargs: _*) ; true })
  }
}
