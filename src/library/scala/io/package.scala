package scala

import java.io._  // { File => JFile, _ }
import java.net.URL
import java.nio.file.{ Path => JPath, _ }
import scala.io.stream._
import java.util.concurrent._

package object io {
  import StandardWatchEventKinds._

  type JFile = java.io.File

  type FileEvent = WatchEvent.Kind[_]
  val CreateEvent = ENTRY_CREATE
  val DeleteEvent = ENTRY_DELETE
  val ModifyEvent = ENTRY_MODIFY
  val AllFileEvents = Array[FileEvent](CreateEvent, DeleteEvent, ModifyEvent)

  lazy val spawnPool = Executors.newCachedThreadPool()

  def runnable(body: => Unit): Runnable     = new Runnable { override def run() = body }
  def callable[T](body: => T): Callable[T]  = new Callable[T] { override def call() = body }
  def spawn[T](body: => T): Future[T]       = spawnPool submit callable(body)
  def submit(runnable: Runnable)            = spawnPool submit runnable

  def slurpStream(is: => InputStream)(implicit _codec: Codec): String = {
    val s = new ReadStream {
      def inputStream = is
      def codec = _codec
    }
    s.readAsString()
  }
  def slurpFile(path: String)(implicit codec: Codec): String = new Path(path) slurp
  def slurpFile(file: JFile)(implicit codec: Codec): String  = new Path(file) slurp
  def slurpUrl(url: URL)(implicit codec: Codec): String      = slurpStream(url.openStream())
  def slurpUrl(url: String)(implicit codec: Codec): String   = slurpUrl(new URL(url))

  /** Generate a string using a routine that wants to write on a writer/stream.
   *  For instance, java has methods with these signatures on Throwable:
   *
   *    printStackTrace(PrintStream s)
   *    printStackTrace(PrintWriter s)
   *
   *  This becomes a string with e.g. slurpStream(throwable printStackTrace _)
  */
  def slurpWriter(f: PrintWriter => Unit)(implicit codec: Codec): String = {
    val stringWriter = new StringWriter()
    val printWriter  = new PrintWriter(stringWriter)

    try f(printWriter) finally printWriter.close()
    stringWriter.toString
  }
  def slurpStreamBytes(f: PrintStream => Unit): Array[Byte] = {
    val byteStream  = new ByteArrayOutputStream()
    val printStream = new PrintStream(byteStream)

    try f(printStream) finally printStream.close()
    byteStream.toByteArray
  }
  def slurpStack(ex: Throwable): String = slurpWriter(ex printStackTrace _)
}
