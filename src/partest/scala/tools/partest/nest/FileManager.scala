/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Philipp Haller
 */

package scala.tools.partest
package nest

import java.io.{ FilenameFilter, IOException,
                FileInputStream, FileOutputStream, BufferedReader,
                FileReader, PrintWriter, FileWriter}
import java.net.URI
import scala.tools.nsc.io.{ Path, Directory, File => SFile }
import scala.collection.mutable
import scala.tools.nsc.util.{ ScalaClassLoader, Exceptional }
import ClassPath.join

trait FileManager extends PartestPaths {
  private lazy val parentLoader = ScalaClassLoader.fromURLs(allLibs)
  def buildPrefix: String

  // def repoRoot     = testRootDir.parent.toAbsolute
  // def testRootDir  = Directory(testRootName).toAbsolute
  // def testRootName = propOrNone("partest.root") getOrElse sys.error("Not set: partest.root")
  // def srcDirName   = propOrElse("partest.srcdir", "files")

  override def toString = s"FileManager(libOrClassesDir=$libOrClassesDir, root=$testRootDir)"

  def libOrClassesDir = (
    List("lib", "classes")
      map (s => Directory(repoRoot / buildPrefix / s))
      collectFirst { case d: Directory if d.isDirectory => d }
      getOrElse sys.error("No build found")
  )
  def latestLibs = libOrClassesDir.list.toList collect { case p if p.isDirectory || p.hasExtension("jar") => p.toURL }

  def javaClassPath = PathResolver.Environment.javaUserClassPath
  def universalLibs = extraTestLibs
  private def isName(url: URL, name: String) = (url.getPath endsWith s"scala-$name.jar") || (url.getPath endsWith "/" + name)
  def latestLib     = latestLibs find (url => isName(url, "library")) orNull
  def latestReflect = latestLibs find (url => isName(url, "reflect")) orNull
  // def actualDir = (
  //   List("lib", "classes")
  //     map (s => Directory(repoRoot / buildPrefix / s))
  //     find (_.isDirectory)
  // )
  // def finishClassGrouping(name: String): URL = {
  //   val actual = actualDir getOrElse { return null }
  //   def lib = actual / s"scala-$name.jar"
  //   def dir = actual / name

  //   if (lib.isFile) lib.toURL else dir.toURL
  // }
  var updateCheck = false
  var failed = false

  def javaCmd: String        = propOrElse("partest.javacmd", "java")
  def javacCmd: String       = propOrElse("partest.javac_cmd", "javac")
  def javaOptsProp: String   = propOrElse("partest.java_opts", "")
  def scalacOptsProp: String = propOrElse("partest.scalac_opts", "")

  final def allLibs         = universalLibs ++ latestLibs
  def newLoader(urls: URL*) = ScalaClassLoader.fromURLs(urls.toList, parentLoader)

  def javaOpts: List[String]   = words(javaOptsProp)
  def scalacOpts: List[String] = words(scalacOptsProp)
  def universalClasspath       = join(ClassPath.fromURLs(allLibs: _*), javaClassPath)

  /** Only when --debug is given. */
  lazy val testTimings = new mutable.HashMap[String, Long]
  def recordTestTiming(name: String, milliseconds: Long) =
    synchronized { testTimings(name) = milliseconds }

  def getLogFile(dir: File, fileBase: String, kind: String): File =
    new File(dir, fileBase + "-" + kind + ".log")

  def getLogFile(file: File, kind: String): File = {
    val dir      = file.getParentFile
    val fileBase = basename(file.getName)

    getLogFile(dir, fileBase, kind)
  }

  def logFileExists(file: File, kind: String) =
    getLogFile(file, kind).canRead

  def overwriteFileWith(dest: File, file: File) =
    dest.isFile && copyFile(file, dest)

  def copyFile(from: File, dest: File): Boolean = {
    if (from.isDirectory) {
      assert(dest.isDirectory, "cannot copy directory to file")
      val subDir:Directory = Path(dest) / Directory(from.getName)
      subDir.createDirectory()
      from.listFiles.toList forall (copyFile(_, subDir))
    }
    else {
      val to = if (dest.isDirectory) new File(dest, from.getName) else dest
      try { to writeAll from.fileContents ; true }
      catch { case _: IOException => false }
    }
  }
}
