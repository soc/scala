package scala
package reflect
package io
package classpath

import java.lang.Character.isJavaIdentifierStart
import java.util.zip.{ ZipFile, ZipInputStream }
import java.util.jar.{ JarInputStream }
import ClassPath._
import scala.collection.{ mutable, immutable, generic }
import java.util.concurrent.ConcurrentSkipListSet

sealed trait Single extends Common {
  def root: FileRep
  def reps: Seq[FileRep]
  def packages: Seq[PackageRep]

  def uri       = root.uri
  def isEmpty   = reps.isEmpty
  def isError   = false
  def summary_a = "%5s files  %3s packages".format(reps.length, packages.length)
  def summary   = "%25s  %s".format(summary_a, root)

  override def toString = summary
}

sealed abstract class SingleJarOrDir extends Single {
  def classes  = reps filter (_.isClass)
}

case class SingleDir(root: FileRep, reps: Seq[FileRep], packages: Seq[PackageRep]) extends SingleJarOrDir {
  def findBytes(path: String): Bytes = root / path match {
    case f if f.isReadableFile => readFromStream(f.fileInputStream(), f.fileLength)
    case _                     => NoBytes
  }
}
case class SingleJar(root: FileRep, reps: Seq[FileRep], packages: Seq[PackageRep]) extends SingleJarOrDir {
  private val zipFile = new ZipFile(root.path)
  def findBytes(path: String): Bytes = zipFile getEntry path match {
    case null  => NoBytes
    case entry => readFromStream(zipFile getInputStream entry, entry.getSize.toInt)
  }
}
case class SingleError(root: FileRep, errorMessage: String) extends {
  val packages = Vector()
  val reps     = Vector()
} with Single {
  override def isError = true
  def findBytes(path: String): Bytes = NoBytes
  override def summary_a = ""
  override def summary = super.summary + " (" + errorMessage + ")"
  override def toString = summary
}


object Single {
  private val useParallel = sys.props contains "cp.parallel"
  private def exHandler(root: FileRep): PartialFunction[Throwable, Single] = {
    case ex @ (_: RuntimeException | _: IOException) => SingleError(root, ex.getMessage stripPrefix root.path + " ")
  }
  def apply(root: jFile): Single   = apply(fileRep(root))
  def apply(root: FileRep): Single = timed((s: Single) => s.summary)(try singleDispatch(root) catch exHandler(root))

  private def isEnterableDir(f: FileRep) = f.name match {
    case "META-INF" => false
    case s          => isJavaIdentifierStart(s charAt 0)
  }

  private def singleCollector(root: FileRep): Single = {
    val reps = Vector.newBuilder[FileRep]
    val pkgs = mutable.HashSet[PackageRep]()

    def addPkg(rep: FileRep): Unit      = pkgs += rep.packageName
    def addJar(jar: FileRep): Unit      = jar foreachZipEntry addEntry
    def addDir(dir: FileRep): Unit      = if (isEnterableDir(dir)) dir.contents foreach add
    def addFile(rep: FileRep): Unit     = { reps += rep ; if (rep.isClass) addPkg(rep) }
    def addEntry(entry: ZipEntry): Unit = if (!entry.isDirectory) add(fileRep(entry.getName))
    def add(rep: FileRep): Unit         = if (rep.isDir) addDir(rep) else addFile(rep)

    if (root.isJarOrZip) {
      addJar(root)
      SingleJar(root, reps.result, pkgs.toVector)
    }
    else {
      root.contents foreach add
      SingleDir(root, reps.result, pkgs.toVector)
    }
  }

  private def singleDispatch(root: FileRep): Single = (
    if (!root.file.exists) SingleError(root, "No such path")
    else if (!root.file.canRead) SingleError(root, "Permission denied")
    else singleCollector(root)
  )

  def singles(roots: Seq[FileRep]): Seq[Single] = (
    if (useParallel) (roots.par map apply).seq
    else roots map apply
  )
}
