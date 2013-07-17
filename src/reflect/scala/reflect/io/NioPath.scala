package scala
package reflect
package io

/** A backport/translation of the java7 java.nio.file.Path interface.
 *  The method signatures are identical, but all referenced types have
 *  been rendered abstract. This interface can coexist with Path where
 *  available and serve as a bridge to the future where it isn't.
 */
trait NioPath extends Any {
  type This <: NioPath with Path
  type Path
  type FileSystem
  type WatchService
  type WatchKind
  type WatchKey
  type WatchModifier
  type LinkOption

  def compareTo(other: Path): Int
  def endsWith(other: String): Boolean
  def endsWith(other: Path): Boolean
  def getFileName(): This
  def getFileSystem(): FileSystem
  def getName(index: Int): This
  def getNameCount(): Int
  def getParent(): This
  def getRoot(): This
  def isAbsolute(): Boolean
  def iterator(): java.util.Iterator[Path]
  def normalize(): This
  def register(watcher: WatchService, events: WatchKind*): WatchKey
  def register(watcher: WatchService, events: Array[WatchKind], modifiers: WatchModifier*): WatchKey
  def relativize(other: Path): This
  def resolve(other: String): This
  def resolve(path: Path): This
  def resolveSibling(other: String): This
  def resolveSibling(other: Path): This
  def startsWith(other: String): Boolean
  def startsWith(other: Path): Boolean
  def subpath(beginIndex: Int, endIndex: Int): This
  def toAbsolutePath(): This
  def toFile(): java.io.File
  def toRealPath(options: LinkOption*): This
  def toUri(): java.net.URI
}

trait NioPathJava6 extends Any with NioPath {
  type This <: NioPathJava6

  type Path          = NioPathJava6
  type FileSystem    = Nothing
  type WatchService  = Nothing
  type WatchKind     = Nothing
  type WatchKey      = Nothing
  type WatchModifier = Nothing
  type LinkOption    = Nothing
}
