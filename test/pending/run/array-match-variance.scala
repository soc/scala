import scala.util.Random

abstract class Tester {
  def discriminate(a: Any): String

  def f1(a: Any) = discriminate(a)
  def f2(a: Any) = a.isInstanceOf[Array[AnyRef]]
  def f3(a: Any) = a.isInstanceOf[Array[String]]
  def f4(a: Any) = a.isInstanceOf[Array[Array[AnyRef]]]
  def f5(a: Any) = a.isInstanceOf[Array[Array[String]]]
  def f6(a: Any) = a.isInstanceOf[Array[Float]]
  def f7(a: Any) = a.isInstanceOf[Array[_]]

  def to_s(x: AnyRef) = x.getClass.getName

  val a1 = Array(new AnyRef { override def toString = "AnyRef" })
  val a2 = Array("a")
  val a3 = Array(a1)
  val a4 = Array(a2)
  val a5 = Array(5.0f)
  val a6 = Array(new Random { override def toString = "Random" })
  val a7 = "abcdefg"

  def echo(xs: Any*) = println(xs mkString " ")

  def run() {
    for (x <- List(a1, a2, a3, a4, a5, a6, a7)) {
      echo(to_s(x), f1(x), f2(x), f3(x), f4(x), f5(x), f6(x), f7(x))
    }
  }
}

object Test {
  val wildcard = new Tester {
    def discriminate(a: Any) = a match {
      case x: Array[Int]         => "int" + x(0)
      case x: Array[Long]        => "long" + x(0)
      case x: Array[_ <: Random] => "random"
      case x: Array[_ <: AnyRef] => "anyref"
      case x: Array[_ <: Any]    => "any"
      case _                     => "no match"
    }
  }
  val existential = new Tester {
    def discriminate(a: Any) = a match {
      case x: Array[Int]                              => "int" + x(0)
      case x: Array[Long]                             => "long" + x(0)
      case x: (Array[t] forSome { type t <: Random }) => "random"
      case x: (Array[t] forSome { type t <: AnyRef }) => "anyref"
      case x: (Array[t] forSome { type t <: Any })    => "any"
      case _                                          => "no match"
    }
  }

  def main(args: Array[String]): Unit = {
    wildcard.run()
    existential.run()
  }
}
