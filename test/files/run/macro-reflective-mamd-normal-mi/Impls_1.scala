import scala.reflect.macros.blackbox.Context

object Impls {
  def foo(c: Context)(x: c.Expr[Int]) = {
    import c.universe._
    val body = Apply(Select(x.tree, TermName("+")), List(Literal(Constant(1))))
    c.Expr[Int](body)
  }
}
