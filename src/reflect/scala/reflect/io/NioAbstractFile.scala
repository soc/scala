package scala
package reflect
package io

/***  java7

import java.io.BufferedOutputStream
import java.nio.file.{ Files, Paths }
import improving.jio._

class NioAbstractFile(val nioPath: java.nio.file.Path) extends AbstractFile {
  private def mkPath(p: java.nio.file.Path): AbstractFile = new NioAbstractFile(p)

  def name: String = nioPath.getFileName.toString
  def path: String = nioPath.toString
  def canonicalPath: String = nioPath.normalize.toString
  def hasExtension(other: String) = nioPath hasExtension other
  def absolute: AbstractFile = mkPath(nioPath.toAbsolutePath)
  def container: AbstractFile = mkPath(nioPath.getParent)
  def file: java.io.File = nioPath.toFile
  def underlyingSource: Option[AbstractFile] = {
    val str = nioPath.toUri.toString
    str indexOf '!' match {
      case -1  => None
      case idx => Some(mkPath(Paths get (str take idx)))
    }
  }
  def exists: Boolean = nioPath.isReadable
  def isClassContainer: Boolean = nioPath.isDirectory()
  def create(): Unit = ???
  def delete(): Unit = ???
  def isDirectory: Boolean = nioPath.isDirectory()
  def isVirtual: Boolean = false
  def lastModified: Long = 0L
  def input: InputStream = nioPath.inputStream()
  def output: OutputStream = nioPath.outputStream()
  def bufferedOutput: BufferedOutputStream = new BufferedOutputStream(output)
  def sizeOption: Option[Int] = Some(nioPath.size.toInt)
  def toURL: URL = nioPath.toUri.toURL
  def toCharArray: Array[Char] = toByteArray map (_.toChar)
  def toByteArray: Array[Byte] = nioPath.bytes
  def iterator: Iterator[AbstractFile] = nioPath.entries.iterator map mkPath
  def lookupName(name: String, directory: Boolean): AbstractFile = mkPath(nioPath / name)
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = mkPath(nioPath / name)
  def lookupPathUnchecked(path: String, directory: Boolean): AbstractFile = mkPath(nioPath / path)
  def fileNamed(name: String): AbstractFile = mkPath(nioPath / name)
  def subdirectoryNamed(name: String): AbstractFile = mkPath(nioPath / name)
}


***/