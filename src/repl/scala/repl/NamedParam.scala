/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.repl

import scala.tools.nsc._
import language.implicitConversions
import util.TypeStrings
import scala.reflect.{ ClassTag, classTag }
import scala.reflect.runtime.{ universe => ru }
import ru.{ TypeTag }

object NamedParam {
  def apply[T](name: String, tpe: String, value: T): NamedParam[T]    = new UntypedParam(name, tpe, value)
  def apply[T: TypeTag : ClassTag](name: String, x: T): NamedParam[T] = new TypedParam(name, x)
  def apply[T: TypeTag : ClassTag](x: T): NamedParam[T]               = new TypedParam(freshName(), x)

  protected val freshName = {
    var counter = 0
    () => { counter += 1; "p" + counter }
  }

  implicit def namedValue[T: TypeTag : ClassTag](name: String, x: T): NamedParam[T] = apply(name, x)
  implicit def tuple[T: TypeTag : ClassTag](pair: (String, T)): NamedParam[T]       = apply(pair._1, pair._2)
}

abstract class NamedParam[T] {
  def name: String
  def tpe: String
  def value: T
  def tag: TypeTag[T]

  override def toString = name + ": " + tpe
}

class UntypedParam[T](val name: String, val tpe: String, val value: T) extends NamedParam[T] {
  def tag = null
}

class TypedParam[T: TypeTag : ClassTag](val name: String, val value: T) extends NamedParam[T] {
  def tpe = TypeStrings.fromTag[T]
  def tag = implicitly[TypeTag[T]]
}
