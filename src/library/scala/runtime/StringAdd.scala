/*                                                                      *\
**     ________ ___   __   ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ |_|                                         **
**                                                                      **
\*                                                                      */

package scala.runtime

import scala.language.experimental.macros

/** A wrapper class that adds string concatenation `+` to any value */
final class StringAdd(val self: Any) extends AnyVal {
  def +(other: String) = String.valueOf(self) + other
}
final class MacroStringAdd(val pieces: List[String]) extends AnyVal {
  def +(other: String): MacroStringAdd = new MacroStringAdd(pieces :+ other)
  override def toString = pieces.mkString
}

// final class MacroStringAdd(val selves: List[String]) extends AnyVal {
//   def +(other: String): StringAdd = String.valueOf(self) + other
// }

object MacroStringAdd {
  import scala.reflect.macros.Context

  implicit def lowerMacroStringAdd(x: MacroStringAdd): String = x.toString

  def append(x: String): String = macro appendImpl
  def appendImpl(c: Context)(x: c.Expr[String]): c.Expr[String] = {
    import c.universe._
    val Plus: TermName = "$plus"

    def loop(t: Tree): List[Tree] = t match {
      case Apply(Select(lhs, Plus), rhs :: Nil) => loop(lhs) ++ loop(rhs)
      case _                                    => t :: Nil
    }

    val pieces = loop(x.tree)
    val strings = pieces map { t =>
      t match {
        case Literal(Constant(x: String)) => x
        case Ident(name)                  => "$" + name
        case Select(This(_), name)        => "$" + name
        case expr                         =>
          val code = expr.pos.source.content.mkString
          val src = expr.pos.lineContent
          println("" + (expr.pos, expr.pos.source, src))
          "${ " + src + " }"
      }
    }

    c.Expr[String](Literal(Constant(strings.mkString)) setPos c.enclosingPosition)
    // reify(result)
    // strings foreach println
    // println(x)
    // x
    // reify(new MacroStringAdd(x.value))
  }
}
