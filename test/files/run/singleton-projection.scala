package foo {
  object Lorax { object Wog ; class Wog }
  class Lorax  { object Zax ; class Zax }
}

package bar {
  trait T1[A >: Null] {
    trait T2[B] {
      object Bob
      class Bob
      def f(x: B): A = null
      def g(x: B): A
      def h(x: B): A
    }
  }

  trait U1[A >: Null] extends T1[A] {
    trait U2 extends T2[Int] {
      object Tom
      class Tom
      def g(x: Int): A = null
    }
  }

  object X1 extends U1[String] {
    object X2 extends U2 {
      def h(x: Int): String = ""
    }
  }

  object Lorax { object Wog ; class Wog }
  class Lorax  { object Zax ; class Zax }
}

object Test {
  import scala.reflect.runtime.universe._

  class X[A1] { class Y[B1] { class Z[C1] { object Q } } }

  def main(args: Array[String]): Unit = {
    println(typeOf[foo.Lorax.type])
    println(typeOf[foo.Lorax.Wog.type])
    println(typeOf[foo.Lorax.Wog])
    println(typeOf[foo.Lorax])
    println(typeOf[foo.Lorax#Zax.type])
    println(typeOf[foo.Lorax#Zax])

    import bar._
    println(typeOf[(T1[_]#T2[_])#Bob])
    println(typeOf[(T1[_]#T2[_])#Bob.type])

    println(typeOf[(X[_ <: Int]#Y[_ >: Null]#Z[_ <: List[_]])#Q.type])
  }
}
