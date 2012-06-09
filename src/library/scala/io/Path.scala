package scala
package io

import stream._
import java.io.{ File => JFile, _ }
// import java.io.{ File => JFile }

class Path(val file: JFile)(implicit val codec: Codec) {
  def this(path: String) = this(new JFile(path))

  def slurp: String = try in.readAsString() catch { case _: IOException => "" }
  def list = file.listFiles match {
    case null => Iterator.empty
    case xs   => xs.iterator map (x => new Path(x))
  }
  def files                     = list filterNot (_.isDirectory)
  def dirs                      = list filter (_.isDirectory)
  def deepFiles: Iterator[Path] = files ++ dirs.flatMap(_.deepFiles)
  def deepDirs: Iterator[Path]  = dirs ++ dirs.flatMap(_.deepDirs)

  def in = new ReadStream {
    def inputStream() = new FileInputStream(file)
    def codec = Path.this.codec
  }
  def out = new WriteStream {
    def outputStream() = new FileOutputStream(file, /*append = */false)
    def codec = Path.this.codec
  }
  def appendOut = new WriteStream {
    def outputStream() = new FileOutputStream(file, /*append = */true)
    def codec = Path.this.codec
  }

  def appendLines(lines: IterableOnce[String]): this.type = {
    appendOut writeLines lines
    this
  }
  def writeLines(lines: IterableOnce[String]): this.type = {
    out writeLines lines
    this
  }

  def toByteArray(): Array[Byte] = {
    // if we don't know the length, fall back on relative inefficiency
    if (file.length <= 0)
      return Path.this.in.readAsBytes

    val len    = file.length.toInt
    val arr    = new Array[Byte](len)
    val in     = Path.this.in.bufferedInput()
    var offset = 0

    def loop() {
      if (offset < len) {
        val read = in.read(arr, offset, len - offset)
        if (read >= 0) {
          offset += read
          loop()
        }
      }
    }
    try loop()
    finally in.close()

    if (offset == len) arr
    else throw new IOException("Could not read entire source (%d of %d bytes)".format(offset, len))
  }

  override def hashCode = file.hashCode
  override def equals(other: Any) = other match {
    case x: Path  => file == x.file
    case _        => false
  }
  override def toString = "" + file
}

object Path {
  def apply(path: String): Path = new Path(path)
  def apply(file: JFile): Path  = new Path(file)

  implicit def path2readStream(path: Path): ReadStream   = path.in
  implicit def path2writeStream(path: Path): WriteStream = path.out
  implicit def path2javaFile(path: Path): JFile          = path.file
  implicit def string2scalaPath(path: String): Path      = new Path(path)
  implicit def file2scalaPath(file: JFile): Path         = new Path(file)
}
