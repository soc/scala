
/*                                                                      *\
**     ________ ___   __   ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ |_|                                         **
**                                                                      **
\*                                                                      */

package scala.runtime

import scala.language.experimental.macros

// final class StringAddPiece(val self: String) extends AnyVal {
//   override def toString = self
// }
// final class StringAddList(val pieces: List[StringAddPiece]) extends AnyVal {
//   def +(other: String): StringAddList = new StringAddList(pieces :+ new StringAddPiece(other))
//   override def toString = pieces.mkString
// }

object MacroStringAdd {
  import scala.reflect.macros.Context

  implicit def lowerMacroStringAdd(x: MacroStringAdd): String = x.toString

  def macroStringAdd(x: String): String = macro macroStringAddImpl
  def macroStringAddImpl(c: Context)(expr: c.Expr[String]): c.Expr[String] = {
    import c.universe._

    def added(tree: Tree): List[Tree] = {
      val Plus: TermName = "$plus"

      def loop(t: Tree): List[Tree] = t match {
        case Apply(Select(lhs, Plus), rhs :: Nil) => loop(lhs) ++ loop(rhs)
        case _                                    => t :: Nil
      }

      val pieces = loop(x.tree)
    }
    def pieceToInterpolatedString(t: Tree): String = t match {
      case Literal(Constant(x: String)) => x
      case Ident(name)                  => "$" + name
      case Select(This(_), name)        => "$" + name
      case expr                         =>
        val code = expr.pos.source.content.mkString
        val len  = expr.pos.end - expr.pos.start
        val src  = code drop expr.pos.start take len
        // println("" + (expr.pos, expr.pos.source, src))
        "${ " + src + " }"
    }

    val pieces  = added(expr.tree)
    val strings = pieces map pieceToInterpolatedString

    c.Expr[String](Literal(Constant(strings.toString)) setPos c.enclosingPosition)
    // reify(result)
    // strings foreach println
    // println(x)
    // x
    // reify(new MacroStringAdd(x.value))
  }
}
