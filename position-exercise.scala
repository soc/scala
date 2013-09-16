package p1 {
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
}

package p3 {
  class Foo {
    List(1)
    Seq(1)
    List
    Seq
    Nil
  }
}

package p4 {
  object Foo {
    sealed abstract class <:<[-From, +To] extends (From => To) with Serializable { def apply(x: From): To = x.asInstanceOf[To] }
    private[this] final val singleton_<:< = new <:<[Any,Any] { override def apply(x: Any): Any = x }
  }
}

package p5 {
  object Foo {
    class X
    private val y = new X { def z = 1 }
  }
}

package p6 {
  object Foo {
    val Bippy, Boppy, Dingleberry = 55
  }
}

package p7 {
  abstract class A[T] {
    val foo: Set[_ <: T] = null
    val bar: Set[_ <: T]
  }
}

package p8 {
  case class Bounds(lo: Int, hi: Int)
  class A(b: Bounds) {
    val Bounds(lo, hi) = b
    def f = lo + hi
  }
}

