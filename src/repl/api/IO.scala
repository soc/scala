package scala.repl
package api

trait IO {
  def in: InputStream
  def out: OutputStream
  def err: OutputStream
}
