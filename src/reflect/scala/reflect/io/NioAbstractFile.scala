package scala
package reflect
package io

// import scala.collection.JavaConverters._
import java.io.{ File => jFile }

object NioAbstractFile {
  def apply(file: NioPath with AnyRef): NioAbstractFile[file.type] = new NioAbstractFile[file.type](file)
}
import NioAbstractFile._

class NioAbstractFile[A <: NioPath](val nioPath: A) extends AbstractFile {
  private type This     = nioPath.This
  private type Path     = nioPath.Path
  private def forwarder = convert(nioPath.toFile)

  private def convert(f: jFile): AbstractFile = new PlainFile(io.Path(f))
  private def convert(p: This): AbstractFile  = convert(p.toFile)

  def absolute    = convert(nioPath.toAbsolutePath)
  def container   = convert(nioPath.getParent)
  def file        = nioPath.toFile
  def name        = nioPath.getFileName.toString
  def path        = nioPath.toString

  def iterator: Iterator[AbstractFile] = file.listFiles match {
    case null => Iterator.empty
    case xs   => xs.iterator map convert
  }

  def lookupName(name: String, directory: Boolean): AbstractFile          = lookupNameUnchecked(name, directory)
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = convert(nioPath resolve name)

  def isDirectory    = forwarder.isDirectory
  def lastModified   = forwarder.lastModified
  def input          = forwarder.input
  def output         = forwarder.output
  def create(): Unit = forwarder.create()
  def delete(): Unit = forwarder.delete()

  override def toString = nioPath.toString
}