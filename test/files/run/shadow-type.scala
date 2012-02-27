// a.scala
// Mon Feb 27 08:20:00 PST 2012

trait Bound {
  type M1 <: String
  type M2 >: String
  type M3
}

abstract class Super[T <: Bound](val term: T) {
  class Inner
}

class Sub[T <: Bound](term: T) extends Super[T](term) {
  protected type S1 = super.term.M1
  protected type S2 = super.term.M2
  protected type S3 = super.term.M3

  def f1: S1 = null.asInstanceOf[S1]
  def f2: S2 = null.asInstanceOf[S2]
  def f3: S3 = null.asInstanceOf[S3]

  class Inner extends super.Inner { }

  def p1(x: Any) = x match {
    case _: Inner       => 1
    case _: super.Inner => 2
    case _              => 3
  }
  def p2() {
    println(p1(new Inner))
    println(p1(new super.Inner))
    println(p1(new Bound { }))
  }

  // Of course, this doesn't work:
  // def g1 = super.term
  // files/run/shadow-type.scala:21: error: super may be not be used on value term
  //   def g1 = super.term
  //                  ^
  // one error found
}

object Test {
  def show[T: Manifest](x: T) = println(manifest[T].erasure.getName)

  def main(args: Array[String]): Unit = {
    val b = new Bound {
      type M1 = String
      type M2 = AnyRef
      type M3 = List[Int]
    }

    val x = new Sub(b)
    show(x.f1)
    show(x.f2)
    show(x.f3)

    x.p2()
  }
}
