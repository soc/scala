import scala.reflect.makro.Context
import collection.mutable.ListBuffer
import collection.mutable.Stack
import language.experimental.macros

package scala

object HListMacro {
  def makeHList(args: Any*): HList = macro makeHList_impl
  def makeHList_impl(c: Context)(params: c.Expr[Any]*) = {
    import c.universe._

    val args = params.toList map (_.tree)
    val tpes = args map (x => c.typeCheck(x.duplicate).tpe)

    // val hlistType = tpes.foldRight(HNilClass.tpe)((tpe, res) => appliedType(HConsClass, List(tpe, res)))
    val hlistExpr =
      args.foldRight(treeBuild.mkAttributedRef(HNilClass))((arg, res) =>
        treeBuild.mkMethodCall(HConsClass, newTermName("apply"), Nil, List(arg, res))
      )

    // c.Expr[Any](hlistExpr

    // Woodsy the Owl says "Type-safety first!"
    c.Expr[Unit](Block(validation :+ call: _*))
  }
}
