package scala
package reflect
package io

import java.nio.file.{ Path => jPath }

trait NioPathJava7 extends jPath with NioPath {
  type This <: NioPathJava7

  type Path          = jPath
  type FileSystem    = java.nio.file.FileSystem
  type WatchService  = java.nio.file.WatchService
  type WatchKind     = java.nio.file.WatchEvent.Kind[_]
  type WatchKey      = java.nio.file.WatchKey
  type WatchModifier = java.nio.file.WatchEvent.Modifier
  type LinkOption    = java.nio.file.LinkOption
}

// Also serves to obtain scalac verification that a class with these methods
// is concrete, showing there are no signature mismatches between the backported
// interface and the jdk one.
class NioPathJava7Forwarder(val path: jPath) extends NioPathJava7 {
  type This = NioPathJava7Forwarder

  private implicit def wrapPath(p: jPath): This = new NioPathJava7Forwarder(p)

  // Unwrapping necessitated by sun using blind casts in the default
  // Path implementation.
  private def unwrap(p: jPath): jPath = p match {
    case x: NioPathJava7Forwarder => unwrap(x.path)
    case _                        => p
  }
  def compareTo(other: Path): Int                  = path compareTo unwrap(other)
  def endsWith(other: Path): Boolean               = path endsWith unwrap(other)
  def endsWith(other: String): Boolean             = path endsWith other
  def getFileName(): This                          = path.getFileName()
  def getName(index: Int): This                    = path.getName(index)
  def getNameCount(): Int                          = path.getNameCount()
  def getParent(): This                            = path.getParent()
  def getRoot(): This                              = path.getRoot()
  def isAbsolute(): Boolean                        = path.isAbsolute()
  def iterator(): java.util.Iterator[Path]         = path.iterator()
  def normalize(): This                            = path.normalize()
  def relativize(other: Path): This                = path relativize unwrap(other)
  def resolve(other: Path): This                   = path resolve unwrap(other)
  def resolve(other: String): This                 = path resolve other
  def resolveSibling(other: Path): This            = path resolveSibling unwrap(other)
  def resolveSibling(other: String): This          = path resolveSibling other
  def startsWith(other: Path): Boolean             = path startsWith unwrap(other)
  def startsWith(other: String): Boolean           = path startsWith other
  def subpath(beginIndex: Int,endIndex: Int): This = path.subpath(beginIndex, endIndex)
  def toAbsolutePath(): This                       = path.toAbsolutePath()
  def toFile(): java.io.File                       = path.toFile()
  def toUri(): java.net.URI                        = path.toUri()
  def getFileSystem(): FileSystem                  = path.getFileSystem()
  def toRealPath(options: LinkOption*): This       = path.toRealPath(options: _*)

  def register(watcher: WatchService, events: WatchKind*): WatchKey                                  = path.register(watcher, events: _*)
  def register(watcher: WatchService, events: Array[WatchKind], modifiers: WatchModifier*): WatchKey = path.register(watcher, events, modifiers: _*)

  override def toString = path.toString
}
