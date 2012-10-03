package improving

import scala.reflect.macros.Context
import scala.language.experimental.macros
import MacroStringAdd._

final class StringAddOps(val value: Any) extends AnyVal {
  def +!(other: Any): String = ???
}

class StringAddContext[T <: Context](val c: T) {
  import c.universe._

  val code  = c.enclosingUnit.source.content.mkString
  println("code = " + code)
  val Plus: TermName = "$plus$bang"

  def apply[T: c.WeakTypeTag](incoming: c.Expr[T]): Unit = { //: c.Expr[StringAddOps] = {
    def codeFor(t: Tree): String = {
      val pos = t.pos
      println("Looking for code for " + t + " at " + t.pos)

      val start = pos.startOrPoint
      val end   = pos.endOrPoint
      val len   = start - end
      code drop start take len
    }
    def added(tree: Tree): List[Tree] = {
      def loop(t: Tree): List[Tree] = t match {
        case Apply(Select(lhs, Plus), rhs :: Nil) => loop(lhs) ++ loop(rhs)
        case _                                    => t :: Nil
      }
      loop(tree)
    }
    def interpolated(t: Tree): String = t match {
      case Literal(Constant(x: String)) => x
      case Ident(name)                  => "${" + name + "}"
      case _                            => "${" + codeFor(t) + "}"
    }
    val pieces = added(incoming.tree)
    val mapped = pieces map interpolated
    println(mapped.mkString)
    // val result = atPos(c.enclosingPosition)(Literal(Constant(mapped.mkString)))
    // val expr   = c.Expr[String](result)
    // reify(new StringAddOps(expr.splice))
    // reify(new StringAddOps(incoming.splice))
  }
}


object MacroStringAdd {
  implicit def macroStringAdd[T >: Null](incoming: T): StringAddOps = macro macroStringAddImpl[T]
  def macroStringAddImpl[T >: Null : c.WeakTypeTag](c: Context)(incoming: c.Expr[T]): c.Expr[StringAddOps] = {
    import c.universe._
    val ctx = new StringAddContext[c.type](c)
    ctx(incoming)
    reify(new StringAddOps(incoming.splice))
  }
}

  //   import c.universe._

  //   println(s"macroStringAddImpl($lhs, $rhs)")
  //   // def added(tree: Tree): List[Tree] = {
  //   //   val Plus: TermName = "$plus"
  //   //   def loop(t: Tree): List[Tree] = t match {
  //   //     case Apply(Select(lhs, Plus), rhs :: Nil) => loop(lhs) ++ loop(rhs)
  //   //     case _                                    => t :: Nil
  //   //   }
  //   //   loop(tree)
  //   // }
  //   def pieceToInterpolatedString(t: Tree): String = t match {
  //     case Literal(Constant(x: String)) => x
  //     case _                            => "${" + t + "}"
  //     // case Ident(name)                  => "${" + name + "}"
  //     // case Select(This(_), name)        => "${" + name + "}"
  //     // case expr                         =>
  //     //   val code = expr.pos.source.content.mkString
  //     //   val len  = expr.pos.end - expr.pos.start
  //     //   val src  = code drop expr.pos.start take len
  //     //   println("" + (expr.pos, expr.pos.source, src))
  //     //   "${ " + src + " }"
  //   }

  //   val result = pieceToInterpolatedString(lhs.tree) + pieceToInterpolatedString(rhs.tree)

  //   // val pieces = added(expr.tree)
  //   // val mapped = pieces map { x => val res = pieceToInterpolatedString(x) ; println("res = " + res) ; res }
  //   // val result = mapped.mkString

  //   c.warning(c.enclosingPosition, result)
  //   c.Expr[String](Literal(Constant(result)) setPos c.enclosingPosition)
  //   // val expr1 = c.Expr[String](Literal(Constant(result)) setPos c.enclosingPosition)
  //   // reify(new SString(expr1.splice))

  //   // c.Expr[String](Literal(Constant(result)) setPos c.enclosingPosition)
  //   // reify(result)
  //   // strings foreach println
  //   // println(x)
  //   // x
  //   // reify(new MacroStringAdd(x.value))
  // }
