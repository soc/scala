package common {
  class A
  class B
}
import common._

package object foo {
  private[foo] type Foo = Object
  private[foo] implicit def bippy(x: A): B = { println("foo.bippy") ; new B }
  private[foo] def dingus(s: String) = { println("foo.dingus " + s) }
}
package foo {
  private[foo] case class DoobieBob(x: Int) { }

  // everything ambiguous, as expected
  class Test {
    import bar._
    println(DoobieBob(5))
    println(new DoobieBob(5))
    def f(x: Foo) = x.length
    val x: B = new A
    bippy(new A)
    dingus("hi")
  }
}

// nothing below here is ambiguous
package object bar {
  type Foo = String
  implicit def bippy(x: A): B = { println("bar.bippy") ; new B }
  def dingus(s: String) = { println("bar.dingus " + s) }
}
package bar {
  case class DoobieBob(x: Int) { }

  class Test {
    import foo._

    println(DoobieBob(5))
    println(new DoobieBob(5))
    def f(x: Foo) = x.length
    val x: B = new A
    bippy(new A)
    dingus("hi")
  }
}

package other {
  class Test {
    import bar._
    import foo._

    println(DoobieBob(5))
    println(new DoobieBob(5))
    def f(x: Foo) = x.length
    val x: B = new A
    bippy(new A)
    dingus("hi")
  }
}
