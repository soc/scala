package scala
package reflect
package io

import java.util.zip.{ ZipEntry, ZipFile, ZipInputStream }
import scala.annotation.tailrec
import scala.collection.{ mutable, immutable, generic }

package object classpath {
  private var lastSlashesIn: String = _
  private var lastSlashesUntil: Int = 0
  private var lastSlashesOut: String = _
  // private var hits = 0
  // private var misses =  0
  // scala.sys addShutdownHook println(s"lastSlash cache $hits hits, $misses misses")

  val NoBytes: Bytes           = Array.emptyByteArray
  val NoFileRep: FileRep       = fileRep("")
  val NoPackageRep: PackageRep = PackageRep("")

  type FileInputStream     = java.io.FileInputStream
  type BufferedInputStream = java.io.BufferedInputStream
  type IOException         = java.io.IOException
  type InputStream         = java.io.InputStream
  type jFile               = java.io.File
  type ZipEntry            = java.util.zip.ZipEntry
  type JarFile             = java.util.jar.JarFile
  type cpMap               = Map[ClassPath, ClassPath]
  type Bytes               = Array[Byte]
  type IISeq[+A]           = immutable.IndexedSeq[A]

  def pathSeparator = java.io.File.pathSeparator

  def extensionOf(name: String): String = {
    var i = name.length - 1
    while (i >= 0 && name.charAt(i) != '.')
      i -= 1

    if (i < 0) ""
    else name.substring(i + 1)
  }
  def stripExtension(name: String): String = name lastIndexOf '.' match {
    case -1  => name
    case idx => name take idx
  }
  def packageOf(path: String): PackageRep = path lastIndexOf '/' match {
    case -1  => NoPackageRep
    case idx => PackageRep(slashesToDots(path, idx))
  }
  def slashesToDots(s: String): String = slashesToDots(s, s.length)
  def slashesToDots(s: String, until: Int): String = {
    def check(): Boolean = {
      def loop(i: Int): Boolean = {
        (i == until) || (
             lastSlashesIn.charAt(i) == s.charAt(i)
          && loop(i + 1)
        )
      }
      (lastSlashesUntil == until) && loop(0)
    }
    if (check()) {
      // hits += 1
      return lastSlashesOut
    }
    // misses += 1
    // return traceres(s"cache hit($s, $until)")(lastSlashesOut)

    val sb = new StringBuilder(until)
    var i = 0
    while (i < until) {
      val ch = s charAt i
      if (ch != '/') sb append ch
      else if (i > 0 && i < until - 1) sb append '.'

      i += 1
    }
    val res = sb.toString
    lastSlashesIn = s
    lastSlashesUntil = until
    lastSlashesOut = res
    res
  }

  def file(s: String): jFile           = new jFile(s)
  def file(f: jFile, s: String): jFile = new jFile(f, s)
  def fileRep(file: jFile): FileRep    = FileRep(file.getPath)
  def fileRep(path: String): FileRep   = FileRep(path)

  def timed[T](msg: T => String)(body: => T): T = {
    val start   = System.nanoTime
    val result  = body
    val elapsed = System.nanoTime - start
    val ms      = "%.3f" format elapsed / 1e6

    println(s"[%8s ms] %s".format(ms, msg(result)))
    result
  }
  def traceres[T](msg: String)(body: T): T = {
    println(msg + ": " + body)
    body
  }
  def trace[T](msg: String)(body: T): T = {
    println(msg)
    body
  }
  def readFromStream(in0: InputStream, len: Int): Bytes = {
    val in = new java.io.BufferedInputStream(in0)
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
