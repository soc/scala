package scala
package reflect
package io
package classpath

import java.net.URL

object ClassRep {
  // private type Path = java.io.File

  // final class SourceFileRep(val path: Path) extends AnyVal
  // final class BinaryFileRep(val path: Path) extends AnyVal

  // implicit class SourceFileRepOps(

  def apply(bin: AbstractFile, src: AbstractFile): ClassRep = (
    if (bin eq NoAbstractFile)
      if (src eq NoAbstractFile) NoClassRep else SourceClassRep(src)
    else
      if (src eq NoAbstractFile) BinaryClassRep(bin) else DualClassRep(bin, src)
  )
}

/**
 * Represents classes which can be loaded with a ClassfileLoader and/or SourcefileLoader.
 */
sealed abstract class ClassRep {
  def bin: AbstractFile
  def src: AbstractFile
  def ext = if (hasBinary) bin.extension else src.extension

  private def isClass = bin hasExtension "class"
  protected final def fail(msg: String) = sys.error(msg)
  def name: String = {
    val name = bin.name
    if (isClass) name.substring(0, name.length - 6)
    else fail("Unexpected binary file ending: " + name)
  }
  def hasBinary     = bin ne NoAbstractFile
  def hasSource     = src ne NoAbstractFile
  def hasBinaryOnly = hasBinary && !hasSource
  def hasSourceOnly = hasSource && !hasBinary
  def hasBoth       = hasBinary && hasSource
  def isEmpty       = !hasBinary && !hasSource

  def updateBinary(bin: AbstractFile): ClassRep = ClassRep(bin, src)
  def updateSource(src: AbstractFile): ClassRep = ClassRep(bin, src)

  override def toString = s"ClassRep(bin=$bin, src=$src)"
}

case class SourceClassRep(src: AbstractFile) extends ClassRep {
  private def isScala = src hasExtension "scala"
  private def isJava  = src hasExtension "java"
  override def name: String = {
    val name = src.name
    if (isScala) name.substring(0, name.length - 6)
    else if (isJava) name.substring(0, name.length - 5)
    else fail("Unexpected source file ending: " + name)
  }
  def bin = NoAbstractFile
}
case class BinaryClassRep(bin: AbstractFile) extends ClassRep {
  def src = NoAbstractFile
}
case class DualClassRep(bin: AbstractFile, src: AbstractFile) extends ClassRep

object NoClassRep extends ClassRep {
  def bin = NoAbstractFile
  def src = NoAbstractFile
}
