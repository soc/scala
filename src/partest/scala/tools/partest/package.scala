/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 */

package scala.tools

import scala.sys.process.javaVmArguments
import java.util.concurrent.Callable
import scala.tools.partest.nest.{ NestUI, Diff, DiffPrint }
import scala.tools.nsc.util.{ ScalaClassLoader, Exceptional }
// import scala.reflect.internal.util.StringOps
// import java.io.{ FileNotFoundException, File => JFile }
// import nsc.io.{ Path, Directory, File => SFile }
// import scala.tools.util.PathResolver
// import nsc.Properties.{ propOrElse, propOrNone, propOrEmpty }
// import scala.sys.process.javaVmArguments
// import java.util.concurrent.Callable

class NoExitSecurityManager extends SecurityManager {
  import java.security.Permission
  override def checkPermission(perm: Permission) = ()
  override def checkPermission(perm: Permission, context: Object) = ()
  override def checkExit(status: Int) = {
    super.checkExit(status)
    throw Exit(status)
  }
}
object NoExitSecurityManager extends NoExitSecurityManager {
  def enable  = System setSecurityManager this
  def disable = System setSecurityManager null
}
case class Exit(code: Int) extends SecurityException with scala.util.control.ControlThrowable

package object partest {
  locally {
    NoExitSecurityManager.enable
    setUncaughtHandler()
  }
  private def cwd = Directory.Current getOrElse sys.error("user.dir property not set")

  def blockingSystemExit(body: => Boolean): Boolean =
    try body catch { case Exit(status) => vlog("Blocked System.exit: " + status) ; status == 0 }

  // Directory <root>/test
  def testRoot: Directory = testRootDir getOrElse {
    def isPartestDir(d: Directory) = (d.name == "test") && (d / srcDirName isDirectory)
    val candidates: List[Directory] = (cwd :: cwd.parents) flatMap (d => List(d, Directory(d / "test")))
    candidates find isPartestDir getOrElse sys.error("Directory 'test' not found.")
  }
  def repoRoot     = testRoot.parent.toAbsolute
  def testRootDir  = testRootName map (n => Directory(n).toAbsolute)
  def testRootName = propOrNone("partest.root") //getOrElse sys.error("Not set: partest.root")
  def srcDirName   = propOrElse("partest.srcdir", "files")
  def srcDir       = Directory(testRoot / srcDirName toCanonical)
  def testBuild    = propOrNone("partest.build")
  def errorCount   = propOrElse("partest.errors", "0").toInt
  def numThreads   = propOrNone("partest.threads").fold(Runtime.getRuntime.availableProcessors)(_.toInt)

  type URL          = java.net.URL
  type File         = java.io.File
  type SFile        = scala.tools.nsc.io.File
  type Path         = scala.tools.nsc.io.Path
  type Directory    = scala.tools.nsc.io.Directory
  type PathResolver = scala.tools.util.PathResolver
  type ClassPath[T] = scala.tools.nsc.util.ClassPath[T]
  type StringWriter = java.io.StringWriter

  val Path         = scala.tools.nsc.io.Path
  val ClassPath    = scala.tools.nsc.util.ClassPath
  val PathResolver = scala.tools.util.PathResolver
  val Directory    = scala.tools.nsc.io.Directory
  val SFile        = scala.tools.nsc.io.File

  def onull(s: String)            = if (s == null) "" else s
  def oempty(xs: String*)         = xs filterNot (x => x == null || x == "")
  def ojoin(xs: String*): String  = oempty(xs: _*) mkString " "
  def nljoin(xs: String*): String = oempty(xs: _*) mkString "\n"

  def setUncaughtHandler() = {
    Thread.setDefaultUncaughtExceptionHandler(
      new Thread.UncaughtExceptionHandler {
        def uncaughtException(thread: Thread, t: Throwable) {
          if (isPartestDebug || sys.props.contains("partest.debug")) {
            val t1 = Exceptional unwrap t
            System.err.println(s"Uncaught exception on thread $thread: $t1")
            t1.printStackTrace()
          }
        }
      }
    )
  }

  implicit class FileOps(f: File) {
    private def sf = SFile(f)

    def testIdent = {
      f.toString split """[/\\]+""" takeRight 2 mkString "/" // e.g. pos/t1234
    }

    def mapInPlace(mapFn: String => String): Unit =
      writeAll(fileLines.map(x => mapFn(x) + "\n"): _*)

    def appendAll(strings: String*): Unit = sf.appendAll(strings: _*)
    def writeAll(strings: String*): Unit = sf.writeAll(strings: _*)
    def absolutePathSegments: List[String] = f.getAbsolutePath split """[/\\]+""" toList

    def isJava        = f.isFile && (sf hasExtension "java")
    def isScala       = f.isFile && (sf hasExtension "scala")
    def isJavaOrScala = isJava || isScala

    def extension = sf.extension
    def hasExtension(ext: String) = sf hasExtension ext
    def changeExtension(ext: String): File = (sf changeExtension ext).jfile

    def fileContents: String    = try sf.slurp() catch { case _: java.io.FileNotFoundException => "" }
    def fileLines: List[String] = augmentString(fileContents).lines.toList
  }

  implicit class PathOps(p: Path) extends FileOps(p.jfile) { }

  implicit def temporaryPath2File(x: Path): File = x.jfile
  implicit def stringPathToJavaFile(path: String): File = new File(path)

  implicit lazy val postfixOps = scala.language.postfixOps
  implicit lazy val implicitConversions = scala.language.implicitConversions

  def fileSeparator = java.io.File.separator
  def pathSeparator = java.io.File.pathSeparator

  def pathToTestIdent(path: Path) = path.jfile.testIdent

  def canonicalizeSlashes(line: String) = {
    line.replaceAll("""[/\\]+""", "/")
  }

  def words(s: String): List[String] = (s.trim split "\\s+").toList

  def timed[T](body: => T): (T, Long) = {
    val t1 = System.currentTimeMillis
    val result = body
    val t2 = System.currentTimeMillis

    (result, t2 - t1)
  }

  def callable[T](body: => T): Callable[T] = new Callable[T] { override def call() = body }

  def file2String(f: File): String = new FileOps(f) fileContents

  def basename(name: String): String = Path(name).stripExtension

  /** In order to allow for spaces in flags/options, this
   *  parses .flags, .javaopts, javacopts etc files as follows:
   *  If it is exactly one line, it is split (naively) on spaces.
   *  If it contains more than one line, each line is its own
   *  token, spaces and all.
   */
  def readOptionsFile(file: File): List[String] = {
    file.fileLines match {
      case x :: Nil   => words(x)
      case xs         => xs map (_.trim)
    }
  }

  def findProgram(name: String): Option[File] = {
    val pathDirs = sys.env("PATH") match {
      case null => List("/usr/local/bin", "/usr/bin", "/bin")
      case path => path split "[:;]" filterNot (_ == "") toList
    }
    pathDirs.iterator map (d => new File(d, name)) find (_.canExecute)
  }

  /**
   * Compares two files using a Java implementation of the GNU diff
   * available at http://www.bmsi.com/java/#diff.
   *
   * @param  f1  the first file to be compared
   * @param  f2  the second file to be compared
   * @return the text difference between the compared files
   */
  def compareFiles(f1: File, f2: File): String = {
    val diffWriter = new StringWriter
    val args = Array(f1.getAbsolutePath(), f2.getAbsolutePath())

    DiffPrint.doDiff(args, diffWriter)
    val res = diffWriter.toString
    if (res startsWith "No") "" else res
  }
  def compareContents(lines1: Seq[String], lines2: Seq[String]): String = {
    val xs1 = lines1.toArray[AnyRef]
    val xs2 = lines2.toArray[AnyRef]

    val diff   = new Diff(xs1, xs2)
    val change = diff.diff_2(false)
    val writer = new StringWriter
    val p      = new DiffPrint.NormalPrint(xs1, xs2, writer)

    p.print_script(change)
    val res = writer.toString
    if (res startsWith "No ") ""
    else res
  }

  def now = (new java.util.Date).toString
  def elapsedString(millis: Long): String = {
    val elapsedSecs = millis/1000
    val elapsedMins = elapsedSecs/60
    val elapsedHrs  = elapsedMins/60
    val dispMins    = elapsedMins - elapsedHrs  * 60
    val dispSecs    = elapsedSecs - elapsedMins * 60

    "%02d:%02d:%02d".format(elapsedHrs, dispMins, dispSecs)
  }

  def vmArgString = javaVmArguments.mkString(
    "Java VM started with arguments: '",
    " ",
    "'"
  )

  def propOrElse(s: String, alt: => String) = sys.props.getOrElse(s, alt)
  def propOrNone(s: String) = sys.props get s
  def propOrEmpty(s: String) = sys.props.getOrElse(s, "")

  def allPropertiesString = {
    import scala.collection.JavaConversions._
    System.getProperties.toList.sorted map { case (k, v) => "%s -> %s\n".format(k, v) } mkString ""
  }

  def showAllJVMInfo() {
    vlog(vmArgString)
    vlog(allPropertiesString)
  }

  def isPartestTerse   = NestUI.isTerse
  def isPartestDebug   = NestUI.isDebug
  def isPartestVerbose = NestUI.isVerbose

  def vlog(msg: => String) = if (isPartestVerbose) System.err.println(msg)

  import scala.language.experimental.macros

  /**
   * `trace("".isEmpty)` will return `true` and as a side effect print the following to standard out.
   * {{{
   *   trace> "".isEmpty
   *   res: Boolean = true
   *
   * }}}
   *
   * An alternative to [[scala.tools.partest.ReplTest]] that avoids the inconvenience of embedding
   * test code in a string.
   */
  def trace[A](a: A) = macro traceImpl[A]

  import scala.reflect.macros.Context
  def traceImpl[A: c.WeakTypeTag](c: Context)(a: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    import definitions._

    // xeno.by: reify shouldn't be used explicitly before the final release of 2.10.0,
    // because this impairs reflection refactorings
    //
    // val exprCode = c.literal(show(a.tree))
    // val exprType = c.literal(show(a.actualType))
    // reify {
    //   println(s"trace> ${exprCode.splice}\nres: ${exprType.splice} = ${a.splice}\n")
    //   a.splice
    // }

    c.Expr(Block(
      List(Apply(
        Select(Ident(PredefModule), newTermName("println")),
        List(Apply(
          Select(Apply(
            Select(Ident(ScalaPackage), newTermName("StringContext")),
            List(
              Literal(Constant("trace> ")),
              Literal(Constant("\\nres: ")),
              Literal(Constant(" = ")),
              Literal(Constant("\\n")))),
          newTermName("s")),
          List(
            Literal(Constant(show(a.tree))),
            Literal(Constant(show(a.actualType))),
            a.tree))))),
      a.tree))
  }
}
