import scala.language.experimental.macros
import scala.reflect.macros._
import Def.Inner

object Test {
  def id[T](t: Inner[T]): Inner[T] = macro idImpl[T]

  def idImpl[T: c.WeakTypeTag](c: Context)(t: c.Expr[Inner[T]]): c.Expr[Inner[T]] =
    c.universe.reify {
      val v: Def.Inner[T] = t.splice
      v
    }
}
