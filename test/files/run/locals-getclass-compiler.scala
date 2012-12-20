import scala.tools.nsc._
import scala.tools.partest.CompilerTest
import scala.collection.{ mutable, immutable, generic }

object Test extends CompilerTest {
  import global._
  import definitions._

def code = """
package extest {
  object Test {
    class Hi
    trait MisterX { override def toString = "X" }
    class Show
    implicit def show[T: Manifest](x: T): T = { println(manifest[T]) ; x }

    val h0 = show { object X extends MisterX ; X }
    val h1 = show { object X extends MisterX ; List(X, X) }
    val h2 = show { object X extends MisterX ; () => X }
    val h3 = show { object X extends MisterX { def bippy(y: Int) = y*y } ; (x: Int) => (y: { def bippy(y: Int): Int }) => y.bippy(x) }

    val s0 = show { class X extends MisterX ; new X }
    val s1 = show { class X extends MisterX ; List(new X, new X) }
    val s2 = show { class X extends MisterX ; () => new X }
    val s3 = show { class X extends MisterX { def bippy(y: Int) = y*y } ; (x: Int) => (y: { def bippy(y: Int): Int }) => y.bippy(x) }
  }
}
"""

  def check(source: String, unit: global.CompilationUnit) = {
    val ms = getMember(getRequiredModule("extest"), newTermName("Test")).moduleClass.info.decls.toList
    ms.map(_.initialize).filter(m => m.isTerm && m.isPublic).sortBy(_.name.toString) foreach { m =>
      afterTyper {
        m.info
        println(m.defString)
        println("    " + erasure.getClassReturnType(m.tpe.finalResultType) + "\n")
      }
    }
    true
  }
}
