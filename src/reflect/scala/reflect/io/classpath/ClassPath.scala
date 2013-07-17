package scala
package reflect
package io
package classpath

import java.net.URL
import scala.collection.{ mutable, immutable }
import scala.reflect.internal.util.StringOps.splitWhere
import File.pathSeparator
import scala.collection.convert.WrapAsScala.enumerationAsScalaIterator
import java.net.MalformedURLException
import java.util.regex.PatternSyntaxException
import scala.util.Properties._

trait ClassPath {
  def isEmpty = asClasspathString == ""
  def name: String
  def origin: Option[String]
  def asURLs: List[URL]
  def asClasspathString: String
  def context: ClassPath.ClassPathContext
  def entries: Seq[ClassPath]
  def classes: Seq[ClassRep]
  def packages: Seq[ClassPath]
  def sourcepaths: Seq[AbstractFile]
  def findClass(name: String): ClassRep
  def findSourceFile(name: String): AbstractFile
  def sortString: String
  def show(): Unit
}

/** <p>
 *    This module provides star expansion of '-classpath' option arguments, behaves the same as
 *    java, see [http://java.sun.com/javase/6/docs/technotes/tools/windows/classpath.html]
 *  </p>
 *
 *  @author Stepan Koltsov
 */
object ClassPath {
  val empty: ClassPath = apply("")

  // def javaUserClassPath: ClassPath = apply(propOrElse("java.class.path", "."))
  def appClassPath: ClassPath = apply(propOrElse("java.class.path", ""))
  // def defaultClassPath = apply(javaUserClassPath)

  def apply(path: String): ClassPath = {
    // def basis = List[Traversable[ClassPath[AbstractFile]]](
    //   classesInPath(javaBootClassPath),             // 1. The Java bootstrap class path.
    //   contentsOfDirsInPath(javaExtDirs),            // 2. The Java extension class path.
    //   classesInExpandedPath(javaUserClassPath),     // 3. The Java application class path.
    //   classesInPath(scalaBootClassPath),            // 4. The Scala boot class path.
    //   contentsOfDirsInPath(scalaExtDirs),           // 5. The Scala extension class path.
    //   classesInExpandedPath(userClassPath),         // 6. The Scala application class path.
    //   classesInManifest(useManifestClassPath),      // 8. The Manifest class path.
    //   sourcesInPath(sourcePath)                     // 7. The Scala source path.
    // )

    // lazy val containers = basis.flatten.distinct

    // expandPath(path) map classesInPath

    // val singles = expandPath(path) map (p => new DirectoryClassPath(new PlainFile(p), DefaultJavaContext))
    // singles foreach println
    new JavaClassPath(DefaultJavaContext.classesInExpandedPath(path), DefaultJavaContext)
  }

  private val ZipMagicNumber = List[Byte](80, 75, 3, 4)
  private def magicNumberIsZip(f: Path) = f.isFile && (f.toFile.bytes().take(4).toList == ZipMagicNumber)

  def isJarOrZip(f: Path): Boolean = isJarOrZip(f, examineFile = true)
  def isJarOrZip(f: Path, examineFile: Boolean): Boolean =
    f.hasExtension("zip", "jar") || (examineFile && magicNumberIsZip(f))

  /** Expand single path entry */
  private def expandS(pattern: String): List[String] = {
    val wildSuffix = File.separator + "*"

    /* Get all subdirectories, jars, zips out of a directory. */
    def lsDir(dir: Directory, filt: String => Boolean = _ => true) =
      dir.list filter (x => filt(x.name) && (x.isDirectory || isJarOrZip(x))) map (_.path) toList

    if (pattern == "*") lsDir(Directory("."))
    else if (pattern endsWith wildSuffix) lsDir(Directory(pattern dropRight 2))
    else if (pattern contains '*') {
      try {
        val regexp = ("^" + pattern.replaceAllLiterally("""\*""", """.*""") + "$").r
        lsDir(Directory(pattern).parent, regexp findFirstIn _ isDefined)
      }
      catch { case _: PatternSyntaxException => List(pattern) }
    }
    else List(pattern)
  }

  /** Split classpath using platform-dependent path separator */
  def split(path: String): List[String] = (path split pathSeparator).toList filterNot (_ == "") distinct

  /** Join classpath using platform-dependent path separator */
  def join(paths: String*): String  = paths filterNot (_ == "") mkString pathSeparator

  /** Split the classpath, apply a transformation function, and reassemble it. */
  def map(cp: String, f: String => String): String = join(split(cp) map f: _*)

  /** Expand path and possibly expanding stars */
  def expandPath(path: String, expandStar: Boolean = true): List[String] =
    if (expandStar) split(path) flatMap expandS
    else split(path)

  /** Expand dir out to contents, a la extdir */
  def expandDir(extdir: String): List[String] = {
    AbstractFile getDirectory extdir match {
      case null => Nil
      case dir  => dir filter (_.isClassContainer) map (x => new java.io.File(dir.file, x.name) getPath) toList
    }
  }

  /** A useful name filter. */
  def isTraitImplementation(name: String) = name endsWith "$class.class"

  def specToURL(spec: String): Option[URL] =
    try Some(new URL(spec))
    catch { case _: MalformedURLException => None }

  /** A class modeling aspects of a ClassPath which should be
   *  propagated to any classpaths it creates.
   */
  abstract class ClassPathContext {
    /** A filter which can be used to exclude entities from the classpath
     *  based on their name.
     */
    def isValidName(name: String): Boolean = true

    /** From the representation to its identifier.
     */
    def toBinaryName(rep: AbstractFile): String

    /** Create a new classpath based on the abstract file.
     */
    def newClassPath(file: AbstractFile): ClassPath

    /** Creators for sub classpaths which preserve this context.
     */
    def sourcesInPath(path: String): List[ClassPath] = (
      for (file <- expandPath(path, expandStar = false) ; dir <- Option(AbstractFile getDirectory file)) yield
        new SourcePath(dir, this)
    )
    def contentsOfDirsInPath(path: String): List[ClassPath] = (
      for (dir <- expandPath(path, expandStar = false) ; name <- expandDir(dir) ; entry <- Option(AbstractFile getDirectory name)) yield
        newClassPath(entry)
    )
    def classesInExpandedPath(path: String): Seq[ClassPath] =
      classesInPathImpl(path, expand = true).toIndexedSeq

    def classesInPath(path: String) = classesInPathImpl(path, expand = false)

    // Internal
    private def classesInPathImpl(path: String, expand: Boolean) =
      for (file <- expandPath(path, expand) ; dir <- Option(AbstractFile getDirectory file)) yield
        newClassPath(dir)

    def classesInManifest(used: Boolean) =
      if (used) for (url <- manifests) yield newClassPath(AbstractFile getResources url) else Nil
  }

  def manifests = Thread.currentThread().getContextClassLoader().getResources("META-INF/MANIFEST.MF").filter(_.getProtocol() == "jar").toList

  class JavaContext extends ClassPathContext {
    def toBinaryName(rep: AbstractFile) = {
      val name = rep.name
      assert(endsClass(name), name)
      name.substring(0, name.length - 6)
    }
    def newClassPath(dir: AbstractFile) = new DirectoryClassPath(dir, this)
  }

  object DefaultJavaContext extends JavaContext {
    override def isValidName(name: String) = !isTraitImplementation(name)
  }

  private[classpath] def endsClass(s: String) = s.length > 6 && s.substring(s.length - 6) == ".class"
  private[classpath] def endsScala(s: String) = s.length > 6 && s.substring(s.length - 6) == ".scala"
  private[classpath] def endsJava(s: String)  = s.length > 5 && s.substring(s.length - 5) == ".java"

  /** From the source file to its identifier.
   */
  def toSourceName(f: AbstractFile): String = (
    if (f hasExtension "scala") f.name dropRight 6
    else if (f hasExtension "java") f.name dropRight 5
    else f.name
  )
}
import ClassPath._

/**
 * Represents a package which contains classes and other packages
 */
abstract class ClassPathImpl extends ClassPath {
  outer =>

  /**
   * The short name of the package (without prefix)
   */
  def name: String

  /**
   * A String representing the origin of this classpath element, if known.
   * For example, the path of the directory or jar.
   */
  def origin: Option[String] = None

  /** A list of URLs representing this classpath.
   */
  def asURLs: List[URL]

  /** The whole classpath in the form of one String.
   */
  def asClasspathString: String

  /** Info which should be propagated to any sub-classpaths.
   */
  def context: ClassPathContext

  /** Lists of entities.
   */
  def classes: Seq[ClassRep]
  def packages: Seq[ClassPath]
  def sourcepaths: Seq[AbstractFile]
  def entries: Seq[ClassPath] = this :: Nil

  /** Filters for assessing validity of various entities.
   */
  def validClassFile(name: String)  = endsClass(name) && context.isValidName(name)
  def validPackage(name: String)    = (name != "META-INF") && (name != "") && (name.charAt(0) != '.')
  def validSourceFile(name: String) = endsScala(name) || endsJava(name)

  /**
   * Find a ClassRep given a class name of the form "package.subpackage.ClassName".
   * Does not support nested classes on .NET
   */
  def findClass(name: String): ClassRep = splitWhere(name, _ == '.', doDropIndex = true) match {
    case Some((pkg, rest)) =>
      packages find (_.name == pkg) match {
        case Some(pkg) => pkg findClass rest
        case _         => NoClassRep
      }
    case _ =>
      classes find (_.name == name) getOrElse NoClassRep
  }

  /** In spectacularly confusing fashion, "findSourceFile" means
   *  to find the binary file.
   */
  def findSourceFile(name: String): AbstractFile = findClass(name) match {
    case rep if rep.hasBinary => rep.bin
    case _                    => NoAbstractFile
  }

  def sortString = join(split(asClasspathString).sorted: _*)
  def show() {
    println("ClassPath %s has %d entries and results in:\n".format(name, entries.size))
    asClasspathString split ':' foreach (x => println("  " + x))
  }
  override def equals(that: Any) = that match {
    case x: ClassPath => this.sortString == x.sortString
    case _                  => false
  }
  override def hashCode = sortString.hashCode()
}

/**
 * A Classpath containing source files
 */
class SourcePath(dir: AbstractFile, val context: ClassPathContext) extends ClassPathImpl {
  def name = dir.name
  override def origin = dir.underlyingSource map (_.path)
  def asURLs = if (dir.file == null) Nil else List(dir.toURL)
  def asClasspathString = dir.path
  val sourcepaths: Seq[AbstractFile] = IndexedSeq(dir)

  private def traverse() = {
    val classBuf   = immutable.Vector.newBuilder[ClassRep]
    val packageBuf = immutable.Vector.newBuilder[SourcePath]
    dir foreach { f =>
      if (!f.isDirectory && validSourceFile(f.name))
        classBuf += ClassRep(NoAbstractFile, f)
      else if (f.isDirectory && validPackage(f.name))
        packageBuf += new SourcePath(f, context)
    }
    (packageBuf.result(), classBuf.result())
  }

  lazy val (packages, classes) = traverse()
  override def toString() = "sourcepath: "+ dir.toString()
}

/**
 * A directory (or a .jar file) containing classfiles and packages
 */
class DirectoryClassPath(val dir: AbstractFile, val context: ClassPathContext) extends ClassPathImpl {
  def name = dir.name
  override def origin = dir.underlyingSource map (_.path)
  def asURLs = if (dir.file == null) List(new URL(name)) else List(dir.toURL)
  def asClasspathString = dir.path
  val sourcepaths: Seq[AbstractFile] = IndexedSeq()

  // calculates (packages, classes) in one traversal.
  private def traverse() = {
    val classBuf   = immutable.Vector.newBuilder[ClassRep]
    val packageBuf = immutable.Vector.newBuilder[DirectoryClassPath]
    dir foreach {
      f =>
        // Optimization: We assume the file was not changed since `dir` called
        // `Path.apply` and categorized existent files as `Directory`
        // or `File`.
        val isDirectory = f match {
          case pf: io.PlainFile => pf.givenPath match {
            case _: io.Directory => true
            case _: io.File      => false
            case _               => f.isDirectory
          }
          case _ =>
            f.isDirectory
        }
        if (!isDirectory && validClassFile(f.name))
          classBuf += ClassRep(f, NoAbstractFile)
        else if (isDirectory && validPackage(f.name))
          packageBuf += new DirectoryClassPath(f, context)
    }
    (packageBuf.result(), classBuf.result())
  }

  lazy val (packages, classes) = traverse()
  override def toString() = "directory classpath: "+ origin.getOrElse("?")
}

class DeltaClassPath(original: ClassPath, subst: cpMap) extends MergedClassPath(original.entries map (e => subst.getOrElse(e, e)), original.context) {
  // not sure we should require that here. Commented out for now.
  // require(subst.keySet subsetOf original.entries.toSet)
  // We might add specialized operations for computing classes packages here. Not sure it's worth it.
}

/**
 * A classpath unifying multiple class- and sourcepath entries.
 */
class MergedClassPath(
  override val entries: Seq[ClassPath],
  val context: ClassPathContext)
extends ClassPathImpl {
  def this(entries: TraversableOnce[ClassPath], context: ClassPathContext) =
    this(entries.toIndexedSeq, context)

  def name = entries.head.name
  def asURLs = (entries flatMap (_.asURLs)).toList
  lazy val sourcepaths: Seq[AbstractFile] = entries flatMap (_.sourcepaths)

  override def origin = Some(entries map (x => x.origin getOrElse x.name) mkString ("Merged(", ", ", ")"))
  override def asClasspathString: String = join(entries map (_.asClasspathString) : _*)

  lazy val classes: Seq[ClassRep] = {
    var count   = 0
    val indices = mutable.HashMap[String, Int]()
    val cls     = new mutable.ArrayBuffer[ClassRep](1024)

    for (e <- entries; c <- e.classes) {
      val name = c.name
      if (indices contains name) {
        val idx      = indices(name)
        val existing = cls(idx)

        if (!existing.hasBinary && c.hasBinary)
          cls(idx) = existing.updateBinary(c.bin)
        if (!existing.hasSource && c.hasSource)
          cls(idx) = existing.updateSource(c.src)
      }
      else {
        indices(name) = count
        cls += c
        count += 1
      }
    }
    cls.toIndexedSeq
  }

  lazy val packages: Seq[ClassPath] = {
    var count   = 0
    val indices = mutable.HashMap[String, Int]()
    val pkg     = new mutable.ArrayBuffer[ClassPath](256)

    for (e <- entries; p <- e.packages) {
      val name = p.name
      if (indices contains name) {
        val idx  = indices(name)
        pkg(idx) = addPackage(pkg(idx), p)
      }
      else {
        indices(name) = count
        pkg += p
        count += 1
      }
    }
    pkg.toIndexedSeq
  }

  private def addPackage(to: ClassPath, pkg: ClassPath) = {
    val newEntries: Seq[ClassPath] = to match {
      case cp: MergedClassPath => cp.entries :+ pkg
      case _                   => IndexedSeq(to, pkg)
    }
    new MergedClassPath(newEntries, context)
  }
  override def toString() = "merged classpath "+ entries.mkString("(", "\n", ")")
}

/**
 * The classpath when compiling with target:jvm. Binary files (classfiles) are represented
 * as AbstractFile. nsc.io.ZipArchive is used to view zip/jar archives as directories.
 */
class JavaClassPath(
  containers: Seq[ClassPath],
  context: JavaContext)
extends MergedClassPath(containers, context) { }

