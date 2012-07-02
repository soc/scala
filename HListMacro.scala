package improving

import scala.reflect.makro.Context
import language.experimental.macros
import shapeless._

object HLists {
  def apply(args: Any*): HList = macro apply_impl
  def apply_impl(c: Context)(args: c.Expr[Any]*) = {
    import c.universe._
    import treeBuild._

    def HNilType    = typeOf[shapeless.HNil.type]
    def HConsModule = typeOf[shapeless.::.type].typeSymbol
    def trees       = args.toList map (_.tree)
    def base        = mkAttributedRef(HNilType.typeSymbol.companionSymbol)

    c.Expr[HList](trees.foldRight(base)((tree, res) => mkMethodCall(HConsModule, newTermName("apply"), Nil, List(tree, res))))
  }
}
