package scala.repl

trait IO {
  def in: InputStream
  def out: OutputStream
  def err: OutputStream
}
