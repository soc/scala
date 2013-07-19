package scala
package reflect
package io
package classpath

import scala.collection.mutable
import java.util.zip.ZipEntry
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
case class SingleZip(basis: PathRep, paths: Seq[PathRep], packages: Seq[PackageRep]) extends Single {
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

  /** This abstraction is necessary or at least prudent to encapsulate the
   *  opening and closing of the zip file.
   */
  private def foreachZipEntry(path: PathRep)(f: ZipEntry => Unit): Unit = {
    def traverse(entries: java.util.Enumeration[_ <: ZipEntry]) {
      while (entries.hasMoreElements)
        f(entries.nextElement)
    }
    val zip = new ZipFile(path.file)
    try traverse(zip.entries) finally zip.close()
  }

  abstract class SingleCollector {
    protected val paths = Vector.newBuilder[PathRep]
    protected val pkgs  = mutable.HashSet[PackageRep]()

    def collect(basis: PathRep): Single

    def addEnclosingPackage(rep: PathRep): Unit = pkgs += PackageRep(rep.path)
    def addFile(rep: PathRep): Unit             = { paths += rep ; addEnclosingPackage(rep) }
    def addDir(dir: PathRep): Unit              = dir.listContents foreach add
    def addZip(rep: PathRep): Unit              = foreachZipEntry(rep)(e => if (!e.isDirectory) addZipEntry(e))
    def addZipEntry(e: ZipEntry): Unit          = addFile(PathRep(e.getName))
    def add(rep: PathRep): Unit                 = if (rep.isDirectory) addDir(rep) else addFile(rep)
  }

  class SingleZipCollector extends SingleCollector {
    def collect(basis: PathRep): SingleZip = {
      addZip(basis)
      SingleZip(basis, paths.result, pkgs.toVector.sorted)
    }
  }

  class SingleDirCollector extends SingleCollector {
    override def addEnclosingPackage(rep: PathRep) {
      if (rep.hasExtension("class") && shouldRecordPackage(rep.parent.filename))
        super.addEnclosingPackage(rep)
    }
    private def shouldRecordPackage(name: String) = (
         (name.length > 0)
      && (name != "META-INF")
      && (isJavaIdentifierStart(name charAt 0))
    )
    def collect(basis: PathRep): SingleDir = {
      addDir(basis)
      SingleDir(basis, paths.result, pkgs.toVector.sorted)
    }
  }

  private def singleCollector(basis: PathRep): Single = (
    if (basis.isJarOrZip)
      new SingleZipCollector collect basis
    else
      new SingleDirCollector collect basis
  )
}
