package p2 {
  class Foo(val s1: String)
  class Bar(s1: String)(s2: Int)(s3: String)
  class Baz(s1: String)(s2: Int*)(s3: String)

  class A {
    def f1 = new Foo
    def f2 = new Bar
    def f3 = new Baz
  }
}

package p9 {
  import annotation._
  class ComplexAnnotation(val value: Annotation) extends ClassfileAnnotation

  class A {
    // It's hard to induce this error because @ComplexAnnotation(@inline) is a parse
    // error so it never gets out of the parser, but:
    @ComplexAnnotation(new inline) def bippy(): Int = 1
  }
}
