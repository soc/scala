import scala.reflect.runtime.universe._

object Test {
  def f[T: TypeTag](xs: List[T]) = xs match {
    case xs: List[String] => 1
    case xs: List[Int]    => 2
  }
  def g[T](xs: List[T]) = xs match {
    case xs: List[String] => 1
    case xs: List[Int]    => 2
  }

  // def f[T: TypeTag](xs: List[T]) = xs match {
  //   case (s: String) :: tail => 1
  // }

  def main(args: Array[String]): Unit = {
    println(f(List("a")))
    println(f(List(1)))
    println(f(List[String]()))
    println(f(List[Int]()))
    println(g(List("a")))
    println(g(List(1)))
    println(g(List[String]()))
    println(g(List[Int]()))
  }
}
