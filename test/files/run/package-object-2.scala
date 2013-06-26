package foo {
  package object bar {
    trait TraversableOps[T] {
      trait Otherwise[R] {
        def otherwise(notbody: => R): R
      }
    }
    val bippy = 55
  }
  package object bing {
    case class Bippy(x: Int) {
      case class Dingus(y: Int) { }
    }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val x = new foo.bar.TraversableOps[String] { }
    class Bippy extends x.Otherwise[Int] {
      def otherwise(x: => Int) = x*x
    }
    println(new Bippy otherwise { println("hi") ; 5 })
    println(foo.bar.bippy)
    val z = foo.bing.Bippy(10)
    println(z.Dingus(10))
  }
}
