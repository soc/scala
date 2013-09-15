class Foo(val s1: String)
class Bar(s1: String)(s2: Int)(s3: String)
class Baz(s1: String)(s2: Int*)(s3: String)

class A {
  def f1 = new Foo
  def f2 = new Bar
  def f3 = new Baz
}
