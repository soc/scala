/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala
package io
package stream

import scala.collection.{ mutable, immutable }
import java.net.{ URI, URL }
import java.io._

object Stream {
  def slurp(is: => InputStream)(implicit _codec: Codec): String = {
    val s = new ReadStream {
      def inputStream = is
      def codec = _codec
    }
    s.readAsString()
  }
  def slurpUrl(url: URL)(implicit codec: Codec): String    = slurp(url.openStream())
  def slurpUrl(url: String)(implicit codec: Codec): String = slurpUrl(new java.net.URL(url))

  /** Call a function on something Closeable, finally closing it. */
  def closing[T <: Closeable, U](stream: T)(f: T => U): U =
    try f(stream) finally stream.close()
}
import Stream._

trait Stream {
  def codec: Codec
}

trait ReadStream extends Stream {
  def inputStream(): InputStream
  def reader(): Reader                     = new InputStreamReader(inputStream, codec.charSet)
  def bufferedReader(): BufferedReader     = new BufferedReader(reader())
  def bufferedInput(): BufferedInputStream = new BufferedInputStream(inputStream())

  private def mkClosingIterator[S <: Closeable, T](in: S, end: T)(f: S => T): Iterator[T] =
    Iterator continually f(in) takeWhile (x => (x != end) || { in.close() ; false })

  def bytesIterator(): Iterator[Byte] =
    mkClosingIterator(bufferedInput(), -1)(_.read().toByte)

  def charsIterator(): Iterator[Char] =
    mkClosingIterator(bufferedReader(), -1)(codec wrap _.read()) map (_.toChar)

  def linesIterator(): Iterator[String] =
    mkClosingIterator(bufferedReader(), null: String)(codec wrapString _.readLine())

  def readAsBytes(): Array[Byte]        = bytesIterator().toArray
  def readAsChars(): Array[Char]        = charsIterator().toArray
  def readAsLines(): IndexedSeq[String] = linesIterator().toIndexedSeq
  def readAsString(): String            = charsIterator().mkString
}

trait WriteStream extends Stream {
  def outputStream(): OutputStream

  /** Obtains an OutputStreamWriter wrapped around a FileOutputStream.
   *  This should behave like a less broken version of java.io.FileWriter,
   *  in that unlike the java version you can specify the encoding.
   */
  def writer(): Writer                       = new OutputStreamWriter(outputStream(), codec.charSet)
  def printStream(): PrintStream             = new PrintStream(outputStream(), /*autoFlush = */true)
  def printWriter(): PrintWriter             = new PrintWriter(bufferedWriter(), /*autoFlush = */true)
  def bufferedWriter(): BufferedWriter       = new BufferedWriter(writer())
  def bufferedOutput(): BufferedOutputStream = new BufferedOutputStream(outputStream())

  def writeLines(lines: IterableOnce[String]) =
    closing(bufferedWriter())(out => lines foreach (out write _ + "\n"))

  def writeBytes(bytes: Array[Byte]) =
    closing(bufferedOutput())(_ write bytes)

  def printlnLines(lines: IterableOnce[String]) =
    closing(printWriter())(out => lines foreach (out println _))
}
