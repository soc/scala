package scala

import scala.reflect.makro.Context
import language.experimental.macros

object HListMacro {
  def makeHList(args: Any*): HList = macro makeHList_impl
  def makeHList_impl(c: Context)(args: c.Expr[Any]*) = {
    import c.universe._

    val HNilType  = typeOf[scala.HNil.type]
    val HConsClass = typeOf[scala.HCons[_,_]].typeSymbol

    val trees = args.toList map (_.tree)
    val tpes  = trees map (x => c.typeCheck(x.duplicate).tpe.widen)

    // val hlistType = tpes.foldRight(HNilClass.tpe)((tpe, res) => appliedType(HConsClass, List(tpe, res)))
    val base = ((treeBuild.mkAttributedRef(HNilType.typeSymbol.companionSymbol), HNilType))
    val (hlistTree, hlistType) = (trees zip tpes).foldRight(base) { case ((tree, tpe), (rtree, rtpe)) =>
      (treeBuild.mkMethodCall(HConsClass.companionSymbol, newTermName("apply"), List(tpe, rtpe), List(tree, rtree)),
        appliedType(typeOf[scala.HCons[_,_]].typeConstructor, List(tpe, rtpe)))
    }

    c.warning(NoPosition, "" + ((hlistType, hlistType)))
      //
      //
      // trees.foldRight(treeBuild.mkAttributedRef(HNilClass))((tree, res) =>
      //   treeBuild.mkMethodCall(HConsClass, newTermName("apply"), Nil, List(tree, res))
      // )

    c.Expr[HList](hlistTree)
  }
}
