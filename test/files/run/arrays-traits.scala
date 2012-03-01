import runtime.ScalaRunTime._

trait SuperS[@specialized(AnyRef) T] {
  def arr: Array[T]
  def foo() = arr(0)
  def bar(b: Array[T]) = b(0) = arr(0)
}

class BaseS[@specialized(AnyRef) T](val arr: Array[T]) extends SuperS[T] { }

trait SuperG[T] {
  def arr: Array[T]
  def foo() = arr(0)
  def bar(b: Array[T]) = b(0) = arr(0)
}

class BaseG[T](val arr: Array[T]) extends SuperG[T] { }

object Test {
  val runtime = new scala.runtime.InstrumentedScalaRunTime
  scala.runtime.ScalaRunTime.setRunTime(runtime)

  def main(args: Array[String]) {
    (new BaseS(new Array[String](1)): SuperS[String]).foo
    runtime.printArrayCounts()
    (new BaseS(new Array[String](1)): SuperS[String]).bar(new Array[String](1))
    runtime.printArrayCounts()

    (new BaseG(new Array[String](1)): SuperG[String]).foo
    runtime.printArrayCounts()
    (new BaseG(new Array[String](1)): SuperG[String]).bar(new Array[String](1))
    runtime.printArrayCounts()
  }
}
