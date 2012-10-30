trait Foo {
  self: bar.type =>

  final class V(val x: Int) extends AnyVal
  // ./a.scala:4: error: value class may not be a member of another class
  //   final class V(val x: Int) extends AnyVal
  //               ^
  // one error found
}

object bar extends Foo { }
