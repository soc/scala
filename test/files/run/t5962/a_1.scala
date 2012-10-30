trait Foo {
  self: bar.type =>

  final class V(val x: Int) extends AnyVal
  // ./a.scala:4: error: value class may not be a member of another class
  //   final class V(val x: Int) extends AnyVal
  //               ^
  // one error found

  final case class W(x: Int) extends AnyVal
}

// object bar extends Foo { }

// object Test {
//   import bar._

//   def main(args: Array[String]): Unit = {
//     assert(new V(5) == new V(5))
//     assert(W(5) == W(5))
//   }
// }
