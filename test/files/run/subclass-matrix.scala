trait A {
  protected final def a(): Int = 5
  def a_id() = a()
}
trait B extends A {
  def b() = a()
}
trait C {
  self: A =>

  def c() = a()
}
trait D {
  self: X =>

  def d() = x()
}
trait E {
  self: X with A =>

  def e() = {
    a()
    x()
  }
}
class X {
  protected final def x(): Int = 5
  def x_id() = x()
}
class Y extends A {
  def y() = a()
}
abstract class Z {
  self: A =>

  def z() = a()
}

object Test {
  val xa  = new A { }
  val xb  = new B { }
  val xc  = new C with A { }
  val xd  = new X with D { }
  val xe1 = new X with A with E { }
  val xe2 = new X with E with A { }
  val xx  = new X
  val xy  = new Y
  val xz  = new Z with A { }

  def calls() = {
    xa.a_id()
    xb.b()
    xc.c()
    xd.d()
    xe1.e()
    xe2.e()
    xx.x_id()
    xy.y()
    xz.z()
  }

  def main(args: Array[String]): Unit = calls()

"""
  val clazzes = "ABCDEXYZ".toList map (ch => rootMirror.staticClass(ch.toString))
  clazzes map (c => (c, clazzes filter (c isSubClass _))) >
  clazzes map (c => (c, clazzes filter (c.tpe <:< _.tpe))) >
  clazzes map (c => (c, clazzes filter (c.typeOfThis <:< _.tpe))) >
  clazzes map (c => (c, clazzes filter (c.tpe <:< _.typeOfThis))) >
  clazzes map (c => (c, clazzes filter (c.typeOfThis <:< _.typeOfThis))) >
"""
}
