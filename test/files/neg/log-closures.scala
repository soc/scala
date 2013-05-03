class Foo {
  def z = 123
  def f(xs: List[Int]) = xs map (_ => z)
  def g(xs: List[Int]) = xs map (_ => 1)
}
//
// class Test {
//   def bip(x: Int) = x + 1
//
//   class Foo {
//     var z = 123
//     def f(xs: List[Int]) = xs map (_ => z)
//   }
//
//   def main(args: Array[String]): Unit = {
//     val q = (new Foo).f(List(1))
//
//   }
// }
