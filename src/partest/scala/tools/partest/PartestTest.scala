package s

import scala.reflect.io._
import ThisTest._
import scala.tools.nsc.util.ClassPath.expandDir

class ThisTest private (val testPath: Path, val buildPath: Directory) {
  def classpath = expandDir(repoRoot relativize buildPath path)
  def path = repoRoot relativize testPath
  // simple idents
  def name = path.stripExtension
  def kind = baseDir.segments.last

  // paths
  def repoRoot     = testRoot.parent.toAbsolute
  def testRoot     = baseDir.parents find (_ / "partest" exists) orNull
  def baseDir      = testPath.parent
  def checkFile    = base("check").toFile
  def flagsFile    = base("flags").toFile
  def javaOptsFile = base("javaopts").toFile
  def outDir       = base("obj").toDirectory

  // textual content
  def scalacOpts = slurp(flagsFile)
  def check      = slurp(checkFile)
  def javaOpts   = slurp(javaOptsFile)

  // booleans
  def debug = isSet("debug")

  // constants
  def fileEncoding = "UTF-8"
  def javaCmd      = "java"
  def javacCmd     = "javac"
  def runnerMain   = "scala.tools.nsc.MainGenericRunner"
  def runnerArgs   = "-usejavacp" :: Nil
  def testMain     = "Test"
  def testArgs     = "jvm" :: Nil

  private def isSet(name: String)     = sys.props contains "partest." + name
  private def slurp(f: File)          = f.safeSlurp() getOrElse ""
  private def base(ext: String): Path = baseDir / s"$name.$ext"
  private def javaClassPath           = sys.props("java.class.path")

  override def toString = s"""
    |ThisTest($path) {
    |   repoRoot: $repoRoot
    |  classpath: $classpath
    | scalacOpts: $scalacOpts
    |}""".trim.stripMargin
}

object ThisTest {
  def prop(key: String): String                 = sys.props("partest." + key)
  def apply(path: String, cp: String): ThisTest = new ThisTest(Path(path).toAbsolute, Directory(cp).toAbsolute)

  implicit def implicitThisTest: ThisTest = apply(prop("test-path"), prop("classpath"))
}
