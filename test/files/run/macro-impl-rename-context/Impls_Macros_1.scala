import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo(unconventionalName: Ctx)(x: unconventionalName.Expr[Int]) = {
    import unconventionalName.universe._
    val body = Block(List(
      Apply(Select(Ident(definitions.PredefObject), TermName("println")), List(Literal(Constant("invoking foo..."))))),
      Literal(Constant(())))
    unconventionalName.Expr[Unit](body)
  }
}

object Macros {
  def foo(x: Int) = macro Impls.foo
}
