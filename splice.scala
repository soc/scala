import scala.reflect.api.{ JavaUniverse }
import scala.reflect.base.{ MirrorOf, TreeCreator, Universe }
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.{ universe => ru }
import ru.{ AbsTypeTag, TypeTag, Literal, Constant, reify }
import scala.tools.reflect.Eval

object Test {
  implicit class TreeEval(tree: Universe#Tree)(implicit val mirror: JavaUniverse#Mirror = currentMirror) {
    def expr[T: AbsTypeTag]: mirror.universe.Expr[T] = {
      object creator extends TreeCreator {
        def apply[U <: Universe with Singleton](m: MirrorOf[U]) : U#Tree = {
          assert(m eq mirror, s"Expr defined in $mirror cannot be migrated to $m.")
          tree.asInstanceOf[U#Tree]
        }
      }
      mirror.universe.Expr[T](mirror.asInstanceOf[MirrorOf[mirror.universe.type]], creator)
    }
    def eval[T: TypeTag] : T = expr[T].splice
  }

  def main(args: Array[String]): Unit = {
    val intExpr = Literal(Constant(23)).expr[Int]
    val sumTree = reify(13 + intExpr.splice)
    println(sumTree.eval)
  }
}
