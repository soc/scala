import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo(c: Ctx)(x: c.Expr[Int])(y: c.Expr[Int]) = {
    import c.universe._
    val sum = Apply(Select(x.tree, TermName("$minus")), List(y.tree))
    val body = Apply(Select(Ident(definitions.PredefObject), TermName("println")), List(sum))
    c.Expr[Unit](body)
  }
}