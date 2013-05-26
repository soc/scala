package scala

import scala.collection.{ mutable, immutable, generic }

object StackVar {
  private val record = mutable.Set[StackVar[_]]()

  def safeString(x: Any): String =
    try "" + x catch { case t: Throwable => "" + t }

  def dump() {
    try record.toList.map(safeString).sorted foreach println
    finally record.clear()
  }

  scala.sys addShutdownHook dump()

  def apply[T](initialValue: T, stringFn: T => String): StackVar[T] = {
    val sv = new StackVar[T](initialValue, stringFn)
    record += sv
    sv
  }
}

class StackVar[T] private (initialValue: T, stringFn: T => String) {
  private[this] var values: List[Update] = initialValue :: Nil
  private implicit def mkUpdate(value: T): Update = Update(value)

  private case class Update(value: T) {
    val index             = if (values eq null) 0 else values.length
    val timestamp         = System.currentTimeMillis
    private def date_s    = new java.util.Date(timestamp)
    override def toString = s"#$index $date_s ${stringFn(value)}"
  }
  private def head = values.head
  def index = head.index
  def set(value: T): this.type = {
    values ::= value
    this
  }
  def get: T = values.head.value
  def size = values.size
  def dump() {
    values.reverse foreach println
  }
  override def toString = s"$get (var #$index)"
}
