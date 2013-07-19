package scala
package reflect
package io
package classpath

import java.io.File.separatorChar
import scala.collection.mutable.ListBuffer
import java.io.{ InputStream, BufferedInputStream, FileInputStream }
import PathRep._

/** A value class wrapper around raw path strings. Null is prohibited
 *  by construction.
 */
final class PathRep private (val path: String) extends AnyVal {
  def / (sub: String) = PathRep(path, sub)

  // Derived names
  def baseFileName = dropExtensionOf(filename)
  def className    = path.substring(0, path.length - 6).replace('/', '.') // assumes it's already known to be a class, for performance
  def extension    = extensionOf(filename)
  def filename     = file.getName
  def packageName  = PackageRep(path)

  // Other derived values
  def contents       = contentsOf(path)
  def file           = new jFile(path)
  def listContainers = contents filter (_.isJarOrZip)
  def parent         = PathRep(file.getParent)
  def pathLength     = path.length
  def uri            = file.toURI

  // Boolean tests
  def isDirectory   = !isEmpty && (lastChar == '/' || lastChar == '\\')
  def isEmpty       = pathLength == 0
  def isClass       = (extension equalsIgnoreCase "class")
  def isJarOrZip    = (extension equalsIgnoreCase "jar") || (extension equalsIgnoreCase "zip")
  def isJavaOrScala = (extension equalsIgnoreCase "scala") || (extension equalsIgnoreCase "java")

  // Means to obtain associated Bytes
  def bytesFromFile(): Bytes = file match {
    case f if f.canRead => PathRep.readFromStream(new FileInputStream(f), f.length.toInt)
    case _              => NoBytes
  }
  def bytesFromZip(zipFile: ZipFile): Bytes = zipFile getEntry path match {
    case null  => NoBytes
    case entry => PathRep.readFromStream(zipFile getInputStream entry, entry.getSize.toInt)
  }

  private def lastChar = path charAt pathLength - 1
  override def toString = path
}

object PathRep {
  val NoPathRep: PathRep = new PathRep("")

  def apply(path: String): PathRep = if (path eq null) NoPathRep else new PathRep(path)
  def apply(file: jFile): PathRep  = new PathRep(file.getPath)

  def apply(parent: String, child: String): PathRep = (
    if (parent eq null) apply(child)
    else if (child eq null) apply(parent)
    else apply(new jFile(parent, child))
  )
  def apply(parent: jFile, child: String): PathRep = (
    if (parent eq null) apply(child)
    else if (child eq null) apply(parent)
    else apply(new jFile(parent, child))
  )
  private def file(path: String): jFile = new jFile(path)

  private def dropExtensionOf(name: String): String = name lastIndexOf '.' match {
    case -1  => name
    case idx => name.substring(0, idx)
  }
  private def extensionOf(name: String): String = {
    var i = name.length - 1
    while (i >= 0 && name.charAt(i) != '.')
      i -= 1

    if (i < 0) ""
    else name.substring(i + 1)
  }
  private def contentsOf(path: String): List[PathRep] = file(path).list match {
    case null => Nil
    case xs   =>
      val buf = new ListBuffer[PathRep]()
      xs foreach (x => buf += PathRep(path, x))
      buf.toList
  }

  private def readFromStream(in0: InputStream, len: Int): Bytes = {
    val in  = new BufferedInputStream(in0)
    val arr = new Bytes(len)

    @tailrec def loop(offset: Int): Int = {
      if (offset >= len) offset
      else {
        val read = in.read(arr, offset, len - offset)
        if (read < 0) offset
        else loop(offset + read)
      }
    }
    val offset = try loop(0) finally in.close()
    if (offset == len) arr
    else sys.error("Could not read entire source (%d of %d bytes)".format(offset, len))
  }
}
