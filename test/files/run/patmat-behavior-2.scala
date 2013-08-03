case class Foo(x: Int, ys: String*)

object Test {
  def f1(x: Any) = x match {
    case Foo(x) => true
    case _      => false
  }
  def f2(x: Any) = x match {
    case Foo(x, y) => true
    case _         => false
  }
  def f3(x: Any) = x match {
    case Foo(x, y, zs @ _*) => true
    case _                  => false
  }

  val x1 = Foo(1)
  val x2 = Foo(1, "a")
  val x3 = Foo(1, "a", "b")

  val fs = List[Any => Boolean](f1, f2, f3)
  val xs = List[Foo](x1, x2, x3)

  def main(args: Array[String]): Unit = {
    for ((f, i) <- fs.zipWithIndex ; (x, j) <- xs.zipWithIndex) {
      println(s"f$i(x$j) == ${f(x)}")
    }
  }
}
