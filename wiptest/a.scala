class Foo(val s1: String)
class Bar(s1: String)(s2: Int)(s3: String)
class Baz(s1: String)(s2: Int*)(s3: String)
class Qux() /** With a comment **/

class A {
  def f1 = new Foo("a") // a line comment
  def f2 = new Bar("a")(5)("b")
  def f3 = new Baz("a")()("b")
  def f4 = new Qux
}
