import scala.tools.partest.ReplTest

package foo {
  trait T
  trait U extends T
  class A
  class B extends A
  class C extends B
  class D extends B with T

  class Test() {
    val f0 = classOf[A]
    val f1 = classOf[B]
    val f2 = classOf[C]
    val f3 = classOf[D]
    val f4 = classOf[T]
    val f5 = classOf[U]
    val f6 = { class B extends A with T ; classOf[B] }
    val f7 = { class B extends A with T ; class C extends B ; trait Q ; class D extends C with Q ; classOf[D] }

    val g0 = (new A).getClass
    val g1 = (new B).getClass
    val g2 = (new C).getClass
    val g3 = (new D).getClass
    val g4 = (new T{}).getClass
    val g5 = (new U{}).getClass
    val g6 = { class B extends A with T ; (new B).getClass }
    val g7 = { class B extends A with T ; class C extends B ; trait Q ; class D extends C with Q ; (new D).getClass }

    override def toString = "classOf/getClass tester."
  }
  class Test2() {
    def f0 = classOf[A]
    def f1 = classOf[B]
    def f2 = classOf[C]
    def f3 = classOf[D]
    def f4 = classOf[T]
    def f5 = classOf[U]
    def f6 = { class B extends A with T ; classOf[B] }
    def f7 = { class B extends A with T ; class C extends B ; trait Q ; class D extends C with Q ; classOf[D] }

    def g0 = (new A).getClass
    def g1 = (new B).getClass
    def g2 = (new C).getClass
    def g3 = (new D).getClass
    def g4 = (new T{}).getClass
    def g5 = (new U{}).getClass
    def g6 = { class B extends A with T ; (new B).getClass }
    def g7 = { class B extends A with T ; class C extends B ; trait Q ; class D extends C with Q ; (new D).getClass }

    override def toString = "classOf/getClass tester."
  }
  class Test3() {
    lazy val f0 = classOf[A]
    lazy val f1 = classOf[B]
    lazy val f2 = classOf[C]
    lazy val f3 = classOf[D]
    lazy val f4 = classOf[T]
    lazy val f5 = classOf[U]
    lazy val f6 = { class B extends A with T ; classOf[B] }
    lazy val f7 = ()
    // lazy val f7 = { class B extends A with T ; class C extends B ; trait Q ; class D extends C with Q ; classOf[D] }
    // found   : Class[D(in value f7)]
    // required: Class[D(in lazy value f7)] forSome { type D(in lazy value f7) <: B with ScalaObject with Object with ScalaObject; type B <: foo.A with foo.T with ScalaObject }
    //  class Test3() {
    //             ^


    lazy val g0 = (new A).getClass
    lazy val g1 = (new B).getClass
    lazy val g2 = (new C).getClass
    lazy val g3 = (new D).getClass
    lazy val g4 = (new T{}).getClass
    lazy val g5 = (new U{}).getClass
    lazy val g6 = { class B extends A with T ; (new B).getClass }
    lazy val g7 = { class B extends A with T ; class C extends B ; trait Q ; class D extends C with Q ; (new D).getClass }

    override lazy val toString = "classOf/getClass tester."
  }
}
object Test extends App {
  class Hi
  trait MisterX { override def toString = "X" }

  class Show
  implicit def showType[T: Manifest](x: T): Show = { println(manifest[T]) ; new Show }

  {
    val t = new foo.Test()
    List[Show](t.f0, t.f1, t.f2, t.f3, t.f4, t.f5, t.f6, t.f7)
    List[Show](t.g0, t.g1, t.g2, t.g3, t.g4, t.g5, t.g6, t.g7)
  }

  {
    val t = new foo.Test2()
    List[Show](t.f0, t.f1, t.f2, t.f3, t.f4, t.f5, t.f6, t.f7)
    List[Show](t.g0, t.g1, t.g2, t.g3, t.g4, t.g5, t.g6, t.g7)
  }

  {
    val t = new foo.Test3()
    List[Show](t.f0, t.f1, t.f2, t.f3, t.f4, t.f5, t.f6, t.f7)
    List[Show](t.g0, t.g1, t.g2, t.g3, t.g4, t.g5, t.g6, t.g7)
  }

  val f0: Show = { classOf[Hi] }
  val f1: Show = { class A ; classOf[A] }
  val f2: Show = { class A ; class B extends A ; classOf[B] }
  val f3: Show = { class A ; class B extends A ; class C extends B ; classOf[C] }
  val f4: Show = { class A extends Hi ; classOf[A] }
  val f5: Show = { class A ; trait B ; class C extends A with B ; classOf[C] }
  val f6: Show = { class A extends Hi ; class B extends A ; classOf[B] }

  val g0: Show = { (new Hi).getClass }
  val g1: Show = { class A ; (new A).getClass }
  val g2: Show = { class A ; class B extends A ; (new B).getClass }
  val g3: Show = { class A ; class B extends A ; class C extends B ; (new C).getClass }
  val g4: Show = { class A extends Hi ; (new A).getClass }
  val g5: Show = { class A ; trait B ; class C extends A with B ; (new C).getClass }
  val g6: Show = { class A extends Hi ; class B extends A ; (new B).getClass }
  val g7: Show = { object A ; A.getClass }
  val g8: Show = { object A extends Hi ; A.getClass }
  val g9: Show = { val x = new Hi { def bippy = 5 } ; x.getClass }
  val g10: Show = { object x extends Hi { def bippy = 5 } ; x.getClass }
}
