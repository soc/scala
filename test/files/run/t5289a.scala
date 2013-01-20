class J[T] {
  def f(x: T): T = x
}

object S extends J[List[String]] { }

object Test {
  def main(args: Array[String]): Unit = {
    println(S f Nil length)
  }
}

