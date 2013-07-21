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

// , override val toByteArray: Array[Byte]) extends AbstractFile {
class CompatAbstractFile(val rep: PathRep, bytesFn: => Array[Byte]) extends AbstractFile {
  // println(s"CompatAbstractFile($rep, _)")
  def name          = rep.filename
  override def path = rep.path
  override def file = rep.file

  override lazy val toByteArray = {
    val res = bytesFn
    println(s"CompatAbstractFile($rep, ${res.size} bytes)")
    res
  }
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
class CompatClassPath(cp: NewClassPath) extends ClassPath[AbstractFile] {
  private val rootPathMap           = cp.buildHierarchy()
  private val rootView: PackageView = new PackageView(Name.Root, rootPathMap)
  private val packageViewMap        = mutable.Map[Name.Dotted, PackageView](Name.Root -> rootView)

  val context: ClassPath.ClassPathContext[AbstractFile] = rootView.context

  class PackageView(enclosing: Name.Dotted, root: PathMap) extends ClassPath[AbstractFile] {
    def nextPackage(name: Name.Simple): PackageView = {
      val fqname = enclosing dot name
      packageViewMap.getOrElseUpdate(fqname, new PackageView(fqname, root(name)))
    }
    def fqpackage                        = (enclosing dot root.name).chars
    def name                             = root.name.toString
    def classes: IndexedSeq[AnyClassRep] = root.pathsToClasses map toClassRep
    def packages                         = root.packageNames map nextPackage
    def asURLs                           = Nil
    def asClasspathString                = ""
    def sourcepaths                      = Vector.empty

    private def toClassRep(rep: PathRep): AnyClassRep      = ClassRep(Some(toAbstractFile(rep)), None)
    private def toAbstractFile(rep: PathRep): AbstractFile = new CompatAbstractFile(rep, cp findBytes rep)
    private def toFileSystemPath(path: String): PathRep    = PathRep(path.replace('.', '/') + ".class")

    object context extends ClassPath.ClassPathContext[AbstractFile] {
      def newClassPath(dir: AbstractFile): ClassPath[AbstractFile] = nextPackage(dir.name)
      def toBinaryName(rep: AbstractFile): String                  = rep.name stripSuffix ".class"
    }
  }

  def name                             = rootView.name.toString
  def classes: IndexedSeq[AnyClassRep] = rootView.classes
  def packages                         = rootView.packages

  // object context extends ClassPath.ClassPathContext[AbstractFile] {
  //   def newClassPath(dir: AbstractFile): ClassPath[AbstractFile] = packageViewMap.getOrElseUpdate(
  //    // new CompatClassPath(cp, root(dir.name))
  //   def toBinaryName(rep: AbstractFile): String                  = rep.name stripSuffix ".class"
  // }

  // println(s"+  $root")
  // def name        = root.name.toString
  // def binaryNames = root.paths filter (_ hasExtension "class") //map (_ stripSuffix ".class") map (_.replace('/', '.'))
  // def classes     = binaryNames flatMap toClassRep toVector
  // def packages    = root.children map (c => new CompatClassPath(cp, c)) toVector
  // private def toAbstractFile(rep: PathRep): AbstractFile = new CompatAbstractFile(rep, cp findBytes rep)
  // private def toFileSystemPath(name: String): PathRep    = PathRep(name.replace('.', '/') + ".class")

  // cp findBytes rep match {
  //   case NoBytes => NoAbstractFile
  //   case bytes   => new CompatAbstractFile(rep, bytes)
  // }
  // private def toClassRep(rep: PathRep): Option[AnyClassRep] = Some(ClassRep(Some(toAbstractFile(rep)), None))
  // override def findClass(name: String): Option[AnyClassRep] = toClassRep(toFileSystemPath(name))
  // toAbstractFile(rep) match {
  //   case NoAbstractFile => None
  //   case file           => Some(ClassRep(Some(file), None))
  // }

  def asURLs = Nil
  def asClasspathString = ""
  def sourcepaths       = Vector()
  // private def packages_s = root.children.map(_.name).mkString(", ")
  // override def toString = s"CompatClassPath($root)"
}
