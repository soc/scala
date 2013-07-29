// generate and call a trait constructor $init$
// if there's a recent to do so - otherwise, don't.

trait Foo1 { def f = 1 }              // no init
trait Foo2 { def g = 2 }              // no init
trait Foo3 { val h = 3 }              // init
trait Foo4 { lazy val h1 = 4 }        // no init
trait Foo5 { type Bip; class Bop }    // no init
trait Foo6 { var h2 = 5 }             // init
trait Foo7 { println(getClass.getName) } // init
trait Foo8 { 55 }                     // init (we could look for purity, but there is no motivation)
trait Foo9 { import scala.collection._ } // no init (and no impl class!)

object Foo {
  class InOb extends Foo1
    with Foo2
    with Foo3
    with Foo4
    with Foo5
    with Foo6
    with Foo7
    with Foo8
    with Foo9

  trait TraitInObject extends Foo1
    with Foo2
    with Foo3
    with Foo4
    with Foo5
    with Foo6
    with Foo7
    with Foo8
    with Foo9

  class Finish extends TraitInObject
}

trait Dingo {
  class InTrait extends Foo1
    with Foo2
    with Foo3
    with Foo4
    with Foo5
    with Foo6
    with Foo7
    with Foo8
    with Foo9

  trait TraitInTrait extends Foo1
    with Foo2
    with Foo3
    with Foo4
    with Foo5
    with Foo6
    with Foo7
    with Foo8
    with Foo9

  class Finish extends TraitInTrait
}

class Dongle {
  class InClass extends Foo1
    with Foo2
    with Foo3
    with Foo4
    with Foo5
    with Foo6
    with Foo7
    with Foo8
    with Foo9

  trait TraitInClass extends Foo1
    with Foo2
    with Foo3
    with Foo4
    with Foo5
    with Foo6
    with Foo7
    with Foo8
    with Foo9

  class Finish extends TraitInClass
}


object Test {
  def show(n: Int) {
    val name = "Foo" + n + "$class"
    println(name)

    try Class.forName(name).getDeclaredMethods.map(_.getName).sorted foreach (m => println("  " + m))
    catch { case _: ClassNotFoundException => println("  <no impl class>") }

    println("")
  }
  def main(args: Array[String]): Unit = {
    // $init$ should be present where expected, and not where not.
    1 to 9 foreach show

    // Instantiating variations which mix in those traits
    // If they are generating bogus calls to ungenerated $init$s,
    // we'll get a NoSuchMethod error here.
    new Foo.InOb
    new Foo.TraitInObject { }
    new Foo.Finish

    val dingo = new Dingo { }
    new dingo.InTrait { }
    new dingo.TraitInTrait { }
    new dingo.Finish

    val dongle = new Dongle
    new dongle.InClass
    new dongle.TraitInClass { }
    new dongle.Finish
  }
}
