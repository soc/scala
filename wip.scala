trait Foo[T] {
  def f: T
  def g = f
}
class Bar extends Foo[String] {
  def f = ""
}

object Test {
  def main(args: Array[String]): Unit = {
    println(new Bar g)
  }
}
