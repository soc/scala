class Foo(val x: Int) extends AnyVal

object Test {
  def main(args: Array[String]): Unit = {
    // This works, prints "Foo@5"
    val x = new Foo(5) ; println(x.asInstanceOf[Object])
    // This works, prints 5
    println(5.asInstanceOf[Object])

    println((new Foo(5)).asInstanceOf[Any])
    println((new Foo(5)).asInstanceOf[AnyVal])
    println((new Foo(5)).asInstanceOf[Object])
  }
}
