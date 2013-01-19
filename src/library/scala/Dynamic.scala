/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2010-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** A marker trait that enables dynamic invocations. Instances `x` of this
 *  trait allow method invocations `x.meth(args)` for arbitrary method
 *  names `meth` and argument lists `args` as well as field accesses
 *  `x.field` for arbitrary field names `field`.
 *
 *  If a call is not natively supported by `x` (i.e. if type checking
 *  fails), it is rewritten according to the following rules:
 *
 *  {{{
 *  foo.method("blah")      ~~> foo.applyDynamic("method")("blah")
 *  foo.method(x = "blah")  ~~> foo.applyDynamicNamed("method")(("x", "blah"))
 *  foo.method(x = 1, 2)    ~~> foo.applyDynamicNamed("method")(("x", 1), ("", 2))
 *  foo.field           ~~> foo.selectDynamic("field")
 *  foo.varia = 10      ~~> foo.updateDynamic("varia")(10)
 *  foo.arr(10) = 13    ~~> foo.selectDynamic("arr").update(10, 13)
 *  foo.arr(10)         ~~> foo.applyDynamic("arr")(10)
 *  }}}
 *
 *  As of Scala 2.10, defining direct or indirect subclasses of this trait
 *  is only possible if the language feature `dynamics` is enabled.
 */
trait Dynamic extends Any

object NamedParam {
  implicit def anyref2NamedParam[T <: AnyRef](value: T) = new NamedRefParam[T]("", value)
  implicit def anyval2NamedParam[T <: AnyVal](value: T) = new NamedValParam[T]("", value)
}
sealed abstract class NamedParam {
  outer =>
  
  type ParamType
  def name: String
  def value: ParamType
  def isPrimitive: Boolean

  def refValue = value.asInstanceOf[AnyRef]
  def erasure  = value.getClass
  def named(name: String): NamedParam { type ParamType = outer.ParamType }
  override def toString = name match {
    case ""   => "" + value
    case _    => name + " = " + value
  }
}
final class NamedRefParam[T <: AnyRef](val name: String, val value: T) extends NamedParam {
  type ParamType = T
  def isPrimitive = false
  def named(name: String): NamedRefParam[T] = new NamedRefParam[T](name, value)
}
final class NamedValParam[T <: AnyVal](val name: String, val value: T) extends NamedParam {
  type ParamType = T
  def isPrimitive = true
  def named(name: String): NamedValParam[T] = new NamedValParam[T](name, value)
}

object Dynamic {
  trait Dynamo[+Result] extends Dynamic {
    def dynamicMissingMethod(which: String, method: String)(args: NamedParam*): Result

    def selectDynamic(method: String): Result                        = dynamicMissingMethod("selectDynamic", method)()
    def applyDynamic(method: String)(args: NamedParam*): Result      = dynamicMissingMethod("applyDynamic", method)(args: _*)
    def applyDynamicNamed(method: String)(args: NamedParam*): Result = dynamicMissingMethod("applyDynamicNamed", method)(args: _*)
    def updateDynamic(method: String)(args: NamedParam*): Result     = dynamicMissingMethod("updateDynamic", method)(args: _*)
  }
  
  class Reflecto(val underlying: AnyRef) extends Dynamo[Any] {
    private val clazz   = underlying.getClass
    private val methods = clazz.getMethods.toList
    private def find(name: String, arity: Int) = methods filter (m => m.getName == name && m.getParameterTypes.size == arity) match {
      case x :: Nil => x
      case xs       => sys.error("Unavailable or ambiguous: " + xs)
    }

    def dynamicMissingMethod(which: String, method: String)(args: NamedParam*): Any = {
      val values  = args.map(_.refValue)
      val m       = find(method, args.size)

      which match {
        case "selectDynamic"     => m.invoke(underlying)
        case "applyDynamic"      => m.invoke(underlying, values: _*)
        case "applyDynamicNamed" => m.invoke(underlying, values: _*)
        case "updateDynamic"     => ()
      }
    }
  }

  class Accumulator extends Dynamo[Accumulator] {
    private var calls: List[String] = Nil

    def dynamicMissingMethod(which: String, method: String)(args: NamedParam*): Accumulator = {
      calls ::= (which match {
        case "selectDynamic"     => method
        case "applyDynamic"      => args.mkString(method + "(", ", ", ")")
        case "updateDynamic"     => args.mkString(method + "(", ", ", ")")
      })
      this
    }

    override def toString = calls.reverse mkString "."
  }
}
