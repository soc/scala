package scala.tools.nsc
package util

import java.net.URL
import scala.collection.{ mutable, immutable }
import scala.reflect.io.NoAbstractFile
import io.{ File, Directory, Path, Jar, AbstractFile }
import scala.reflect.internal.util.StringOps.splitWhere
import Jar.isJarOrZip
import File.pathSeparator
import scala.collection.convert.WrapAsScala.enumerationAsScalaIterator
import java.net.MalformedURLException
import java.util.regex.PatternSyntaxException
import improving.paths._
import ClassPath.{ DefaultJavaContext, join }
import improving.paths.{ ClassPath => NewClassPath }

// class FilteredSingle(single: Single, p: PathRep => Boolean) extends Single {
//   def basis                           = single.basis
//   def paths: Seq[PathRep]             = single.paths filter p
//   def packages: Seq[PackageRep]       = single.packages
//   def findBytes(path: PathRep): Bytes = single findBytes path
// }

class CompatAbstractFile(val rep: PathRep, override val toByteArray: Array[Byte]) extends AbstractFile {
  // println(s"CompatAbstractFile($rep, ${toByteArray.length} bytes)")
  def name          = rep.filename
  override def path = rep.path
  override def file = rep.file

  def absolute: AbstractFile  = this
  def container: AbstractFile = this
  def create()                = ???
  def delete()                = ???
  def input                   = null
  def output                  = null
  def isDirectory             = rep.isDirectory
  def iterator                = if (!isDirectory) Iterator.empty else rep.listContents.iterator map (rep => new CompatAbstractFile(rep, NoBytes))
  def lastModified            = 0L

  def lookupName(name: String, directory: Boolean): AbstractFile = null
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = null
}

// cp: NewClassPath
// case class PathMap(name: Name.Simple, paths: List[PathRep], children: List[PathMap]) {
class CompatClassPath(cp: NewClassPath, root: PathMap) extends ClassPath[AbstractFile] {
  // println(s"+  $root")
  def name        = root.name.toString
  def binaryNames = root.paths filter (_ hasExtension "class") //map (_ stripSuffix ".class") map (_.replace('/', '.'))
  def classes     = printResult("classes")(binaryNames flatMap toClassRep toVector)
  def packages    = printResult("packages")(root.children map (c => new CompatClassPath(cp, c)) toVector)

  object context extends ClassPath.ClassPathContext[AbstractFile] {
    def newClassPath(dir: AbstractFile): ClassPath[AbstractFile] = printResult(s"newClassPath($dir)")(new CompatClassPath(cp, root(dir.name)))
    def toBinaryName(rep: AbstractFile): String                  = printResult(s"toBinaryName($rep)")(rep.name stripSuffix ".class")
  }
  private def printResult[T](msg: String)(res: T): T = {
    // println(s"$msg: $res")
    res
  }
  // def context = DefaultJavaContext

  private def toAbstractFile(rep: PathRep): AbstractFile = cp findBytes rep match {
    case NoBytes => NoAbstractFile
    case bytes   => new CompatAbstractFile(rep, bytes)
  }
  private def toClassRep(rep: PathRep): Option[AnyClassRep] = toAbstractFile(rep) match {
    case NoAbstractFile => None
    case file           => Some(ClassRep(Some(file), None))
  }
  override def findClass(name: String): Option[AnyClassRep] = printResult(name)(toClassRep(PathRep(name.replace('.', '/') + ".class")))

  def asURLs = Nil
  def asClasspathString = ""
  def sourcepaths       = Vector()
  private def packages_s = root.children.map(_.name).mkString(", ")
  override def toString = s"CompatClassPath($root)"
}
