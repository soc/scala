object Test {
  trait <[T1, T2] { }

  final val foo = 7

  def g(x: Int) = x match {
    case _: foo.type => true
    case _           => false
  }

  trait Succ[T] {
    type Out
    def apply(x: T): Out
  }

  implicit object One extends Succ[1.type] {
    type Out = 2.type
    def apply(x: 1.type) = 2 // -1
  }

  def f[T](x: T)(implicit succ: Succ[T]) = succ(x)

  def main(args: Array[String]): Unit = {
    println(f(1))
    // println(f(5))
    // test/files/run/dottype.scala:31: error: could not find implicit value for parameter succ: Test.Succ[Int]
    //     println(f(5))
    //              ^
    // one error found
    println((g(1), g(5), g(7)))
  }
}
