package scala
package reflect
package io
package classpath

import scala.collection.mutable
import java.util.zip.ZipEntry
import java.util.jar._
import java.lang.Character.isJavaIdentifierStart

/** A set of paths associated with a single basis, typically a
 *  jar file or a directory containing classes. All classpath
 *  expansion must be performed prior to creating a Single.
 */
trait Single extends ClassPath {
  def basis: PathRep
  override def toString = this.summary
}

case class SingleDir(basis: PathRep, paths: Seq[PathRep], packages: Seq[PackageRep]) extends Single {
  def findBytes(path: PathRep): Bytes = (basis / path.path).bytesFromFile()
}
case class SingleJar(basis: PathRep, paths: Seq[PathRep], packages: Seq[PackageRep]) extends Single {
  private val zipFile = new ZipFile(basis.path)
  def findBytes(path: PathRep): Bytes = path bytesFromZip zipFile
}
case class SingleError(basis: PathRep, errorMessage: String) extends Single {
  def findBytes(path: PathRep) = NoBytes
  def packages                 = Vector()
  def paths                    = Vector()

  override def toString = this.summary + " (" + errorMessage + ")"
}

object Single {
  def apply(basis: PathRep): Single = (
    if (!basis.file.exists) SingleError(basis, "No such path")
    else if (!basis.file.canRead) SingleError(basis, "Permission denied")
    else try timed[Single](_.summary)(singleCollector(basis)) catch { case ex: Exception => singleError(basis, ex) }
  )

  private def singleError(basis: PathRep, ex: Exception): Single = {
    val msg = ex.getMessage match {
      case null => "" + ex
      case msg  => msg stripPrefix basis.path + " "
    }
    SingleError(basis, msg)
  }

  private def foreachJarEntry(path: PathRep)(f: JarEntry => Unit): Unit = {
    def traverse(entries: java.util.Enumeration[_ <: JarEntry]) {
      while (entries.hasMoreElements)
        f(entries.nextElement)
    }
    val jar = new JarFile(path.file)
    try traverse(jar.entries) finally jar.close()
  }

  private def singleCollector(basis: PathRep): Single = {
    val paths = Vector.newBuilder[PathRep]
    val pkgs = mutable.HashSet[PackageRep]()
    def isPossiblePackage(name: String) = name match {
      case "META-INF" => false
      case s          => isJavaIdentifierStart(s charAt 0)
    }

    def addPkg(rep: PathRep): Unit      = pkgs += rep.packageName
    def addJar(jar: PathRep): Unit      = foreachJarEntry(jar)(addEntry)
    def addDir(dir: PathRep): Unit      = dir.contents foreach add
    def addFile(rep: PathRep): Unit     = { paths += rep ; if (rep.isClass) addPkg(rep) }
    def addEntry(entry: ZipEntry): Unit = if (!entry.isDirectory) add(PathRep(entry.getName))

    def add(rep: PathRep): Unit = (
      if (!rep.isDirectory) addFile(rep)
      else if (isPossiblePackage(rep.filename)) addDir(rep)
    )

    if (basis.isJarOrZip) {
      addJar(basis)
      SingleJar(basis, paths.result, pkgs.toVector.sorted)
    }
    else {
      basis.contents foreach add
      SingleDir(basis, paths.result, pkgs.toVector.sorted)
    }
  }
}
