package scala
package reflect
package io

import scala.collection.JavaConverters._
import java.io.{ File => jFile }

object NioJavaFile {
  def apply(f: String): NioJavaFile       = new NioJavaFile(new jFile(f))
  def apply(f: jFile): NioJavaFile        = new NioJavaFile(f)
  def apply(f: io.Path): NioJavaFile      = new NioJavaFile(f.jfile)
  def apply(f: AbstractFile): NioJavaFile = new NioJavaFile(f.file)
}

final class NioJavaFile(val toFile: jFile) extends AnyVal with NioPathJava6 {
  type This = NioJavaFile

  private def mkJava(f: jFile, ss: String*): jFile  = ss.foldLeft(f)(new jFile(_, _))
  private def mkJava(s: String, ss: String*): jFile = mkJava(new jFile(s), ss: _*)

  private def ioFile                    = io.File(toFile)
  private def convert(f: jFile): This   = NioJavaFile(f)
  private def convert(s: String): This  = NioJavaFile(mkJava(s))
  private def convert(f: io.Path): This = convert(f.jfile)

  def compareTo(other: Path)  = toFile.getPath compareTo other.toString
  def endsWith(other: String) = toFile.getPath endsWith other
  def endsWith(other: Path)   = toFile.getPath endsWith other.toString
  def getFileName()           = convert(toFile.getName)
  def getName(index: Int)     = convert(ioFile.segments(index))
  def getNameCount(): Int     = ioFile.segments.size
  def getParent()             = convert(toFile.getParentFile)
  def getRoot()               = convert(jFile.listRoots.head)
  def isAbsolute()            = toFile.isAbsolute
  def iterator()              = (toFile.listFiles match {
    case null => Iterator.empty
    case xs   => (xs.iterator map (x => convert(x): Path))
  }).asJava
  def normalize()             = convert(toFile.getCanonicalFile)
  def relativize(other: Path) = convert(ioFile relativize other.toFile)
  def resolve(other: String)  = convert(mkJava(toFile, other))
  def resolve(other: Path)    = convert(mkJava(toFile, other.toString))

  def resolveSibling(other: String) = getParent() resolve other
  def resolveSibling(other: Path)   = getParent() resolve other
  def startsWith(other: String)     = toFile.getPath startsWith other
  def startsWith(other: Path)       = toFile.getPath startsWith other.toString

  def subpath(beginIndex: Int, endIndex: Int) = ioFile.segments.slice(beginIndex, endIndex) match {
    case Nil     => throw new IllegalArgumentException(s"subpath($beginIndex, $endIndex)")
    case x :: xs => convert(mkJava(x, xs: _*))
  }
  def toAbsolutePath()                 = convert(toFile.getAbsoluteFile)
  def toRealPath(options: LinkOption*) = convert(toFile.getCanonicalFile)
  def toUri(): java.net.URI            = toFile.toURI

  def getFileSystem(): FileSystem = ???
  def register(watcher: WatchService, events: WatchKind*): WatchKey = ???
  def register(watcher: WatchService, events: Array[WatchKind], modifiers: WatchModifier*): WatchKey = ???

  override def toString = toFile.toString
}
