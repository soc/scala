import scala.reflect.runtime.universe._
import scala.collection.{ mutable, immutable }

// Classes for parameter type specificity
class C1
class C2 extends C1
class C3 extends C2

object Helpers {
  val String = typeOf[String]
  val Int    = typeOf[Int]
  val Double = typeOf[Double]
  val Object = typeOf[Object]

  val C1 = typeOf[C1]
  val C2 = typeOf[C2]
  val C3 = typeOf[C3]
}
import Helpers._

abstract class Overload[T: TypeTag](memberNames: String*) {
  def run(): Unit

  private var memberName: String = _

  val prefix = typeOf[T]
  val calls  = mutable.ListBuffer[Call]() // Results accumulator for outcomes of calls to m

  def member = (prefix termMember memberName).asTerm
  def owner  = member.owner
  def alts   = member.alternatives.reverse  // XXX alternatives still showing up in reverse order

  def m(posVargs: Type*): Unit                                        = mkCall(Nil, posVargs)
  def m[T1: TypeTag](posVargs: Type*): Unit                           = mkCall(List(typeOf[T1]), posVargs)
  def m[T1: TypeTag, T2: TypeTag](posVargs: Type*): Unit              = mkCall(List(typeOf[T1], typeOf[T2]), posVargs)
  def m[T1: TypeTag, T2: TypeTag, T3: TypeTag](posVargs: Type*): Unit = mkCall(List(typeOf[T1], typeOf[T2], typeOf[T3]), posVargs)

  case class Call(member: TermSymbol, targs: List[Type], posVargs: Seq[Type]) {
    lazy val result = member.resolveOverloaded(pre = prefix, targs = targs, posVargs = posVargs)
    def result_s    = sig(result)
    def targs_s     = if (targs.isEmpty) "" else targs.mkString("[", ", ", "]")
    def posVargs_s  = posVargs.mkString("(", ", ", ")")
    def inputs_s    = member.name + targs_s + posVargs_s

    def in  = inputs_s
    def out = result_s
  }

  private def mkCall(targs: List[Type], posVargs: Seq[Type]) = {
    calls += Call(member, targs, posVargs)
  }

  def mType(s: Symbol) = if (s == NoSymbol) NoType else s.typeSignatureIn(prefix)
  private def clean(tp: Any) = ("" + tp).replaceAll("scala.Unit", "Unit")

  def defString(m: Symbol): String = {
    // I wonder how to do this without the implicit
    implicit def fix[T <: scala.tools.nsc.Global#Type](x: Type): T = x.asInstanceOf[T]

    (m: Any) match {
      case x: scala.tools.nsc.Global#Symbol => clean(x.defStringSeenAs(mType(m)))
      case _                                => sig(m)
    }
  }
  def sig(m: Symbol): String = clean(mType(m))

  implicit class TypeApi(tpe: Type) {
    def termMember(name: String): TermSymbol = tpe.member(name: TermName).asTerm
  }

  locally {
    memberNames foreach { name =>
      memberName = name
      println(s"\n${owner.kind} ${owner.name} {")
      println(s"  // ${alts.size} alternatives for $name")
      alts map defString foreach (x => println("  " + x))
      println("}\n")

      println("// Resolutions")
      run()

      val fmtString = "%-" + calls.map(_.in.length).max + "s    %s"
      calls.toList foreach (x => println(fmtString.format(x.in, x.out)))
      calls.clear()
    }
  }
}

class Test1 extends Overload[Test1]("f") {
  def f(s: String) { }
  def f[T](x: T, y: T, z: T) { }
  def f[T, U](s: String, y: (T, U)) { }
  def f(x: Int)(y: Int) { }

  def run() {
    m(String)
    m[String](String, String, String)
    m[String, String](String, typeOf[(String, String)])
    m(Int)

    // neg
    m(Double)
    m[String](String, String, Object)
    m[String, String](String, typeOf[(Object, Object)])
    m(typeOf[Any])
  }
}

class Test2 extends Overload[Test2]("f") {
  def f() { }
  def f(x1: C1) { }
  def f(x2: C2) { }
  def f(x3: C3) { }
  def f(x1: C1, x2: C1) { }
  def f(x1: C1, x2: C2) { }
  def f(x1: C1, x2: C3) { }
  def f(x1: C2, x2: C1) { }
  def f(x1: C2, x2: C2) { }
  def f(x1: C2, x2: C3) { }
  def f(x1: C3, x2: C1) { }
  def f(x1: C3, x2: C2) { }
  def f(x1: C3, x2: C3) { }

  def run() {
    m()
    m(C1)
    m(C2)
    m(C3)
    m(C1, C1)
    m(C1, C2)
    m(C1, C3)
    m(C2, C1)
    m(C2, C2)
    m(C2, C3)
    m(C3, C1)
    m(C3, C2)
    m(C3, C3)
  }
}

class Test3 extends Overload[Test3]("f") {
  def f(x1: C1, x2: C3) { }
  def f(x1: C2, x2: C2) { }
  def f(x1: C3, x2: C1) { }

  def run() {
    m(C1, C1)
    m(C1, C2)
    m(C1, C3)
    m(C2, C1)
    m(C2, C2)  // fail, C1/C2 vs. C2/C1
    m(C2, C3)
    m(C3, C1)
    m(C3, C2)
    m(C3, C3)
  }
}

class Test4 {
  class A1 {
    def f(x: List[Int]) { }
    def f[T](x: T) { }
  }
  object A1T extends Overload[A1]("f") {
    def run() {
      m(typeOf[List[Int]])
      m(typeOf[Object])     // <notype> ! A bug I hope; no type arguments inferred.
      m[List[Int]](typeOf[List[Int]])
      m[Object](typeOf[Object])
    }
  }

  A1T
}

class Test5 {
  class A1 {
    def f(x: C3, y: C2) { }
    def g(x: C3, y: C2) { }
  }
  class A2 extends A1 {
    def f(x: C2, y: C3) { }
    def g(x: C2, y: C3) { }
  }
  class A3 extends A2 {
    def f[T1 <: C1, T2 <: C1](x: T1, y: T2) { }
    def g[T1 <: C2, T2 <: C2](x: T1, y: T2) { }
  }

  object A3Test extends Overload[A3]("f", "g") {
    def run() {
      m(C1, C1)
      m(C1, C2)
      m(C1, C3)
      m(C2, C1)
      m(C2, C2)
      m(C2, C3)
      m(C3, C1)
      m(C3, C2)
      m(C3, C3)

      m[C1](C1, C1)
      m[C1, C2](C1, C2)
      m[C2, C2](C2, C2)
      m[C3, C3](C3, C3)
    }
  }

  A3Test
}

class A {
  def h3[T <: C1](x1: T, x2: => T) { }
  def h3[T <: C1](x1: T, x2: T*) { }
  def h3[T >: C2 <: C1](x1: T, x2: T) { }
}

class Varargs {
  def f(x1: C1*) { }
  def f(x1: C2) { }
  def f(x1: C3) { }
  def f(x1: C1, x2: C1) { }
  def f(x1: C2, x2: C2*) { }
  def f(x1: C3, x2: C3) { }

  def g(x1: => C1) { }
  def g(x1: C1*) { }
  def g(x1: C1) { }

  def h(x1: C1, x2: => C3) { }
  def h(x1: C2, x2: C2*) { }
  def h(x1: C3, x2: C1) { }

  def h2[T](x1: C1, x2: => T) { }
  def h2[T](x1: C1, x2: T*) { }
  def h2[T](x1: C1, x2: T) { }

  def h3[T <: C1](x1: T, x2: => T) { }
  def h3[T <: C1](x1: T, x2: T*) { }
  def h3[T >: C2 <: C1](x1: T, x2: T) { }

  class V2 {
    def f(x1: C1, x2: C1) { }
    def f(x1: C2, x2: C2*) { }
    def f(x1: C3, x2: C3) { }
  }
  class V3 extends V2 {
    def f(x1: C1*) { }
    def f(x1: C2) { }
    def f(x1: C3) { }
  }

  object Varargs1 extends Overload[Varargs]("f", "g", "h") {
    def run() {
      m()
      m(C1)
      m(C2)
      m(C3)
      m(C1, C1)
      m(C1, C2)
      m(C1, C3)
      m(C2, C1)
      m(C2, C2)
      m(C2, C3)
      m(C3, C1)
      m(C3, C2)
      m(C3, C3)
    }
  }
  object Varargs2 extends Overload[V3]("f") {
    def run() {
      m()
      m(C1)
      m(C2)
      m(C3)
      m(C1, C1)
      m(C1, C2)
      m(C1, C3)
      m(C2, C1)
      m(C2, C2)
      m(C2, C3)
      m(C3, C1)
      m(C3, C2)
      m(C3, C3)
    }
  }
  object Varargs3 extends Overload[Varargs]("h2", "h3") {
    def run() {
      m[C1]()
      m[C1](C1)
      m[C2](C2)
      m[C3](C3)
      m[C1](C1, C1)
      m[C1](C1, C2)
      m[C1](C1, C3)
      m[C1](C2, C1)
      m[C2](C2, C2)
      m[C2](C2, C3)
      m[C1](C3, C1)
      m[C2](C3, C2)
      m[C3](C3, C3)
    }
  }


  Varargs1
  Varargs2
  Varargs3
}


object Test {
  def main(args: Array[String]) {
    new Test1
    new Test2
    new Test3
    new Test4
    new Test5

    new Varargs
  }
}
