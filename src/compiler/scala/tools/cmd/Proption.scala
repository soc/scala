/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools
package cmd

trait ProptionSet {
  def get(key: String): Option[String]
  def set(key: String, value: String): Unit
}
trait SystemProptionSet extends ProptionSet {
  def get(key: String): Option[String] = Option(System.getProperty(key))
  def set(key: String, value: String): Unit = System.setProperty(key, value)
}

object Proption {
  abstract class Proptional[T](implicit val lens: Lens[T]) {
    type Value = lens.Value
    def name: String
    def value: Value

    def default: Value    = zeroValue
    def errorValue: Value = zeroValue
    def zeroValue: Value  = lens.zeroValue
    def strValue: String  = value.strValue
    def typed: T          = value.typed
    def isSet: Boolean    = value != default
    def orElse(alt: => T): T = if (isSet) typed else alt
  }
  case class TOpt[T: Lens](name: String, override val strValue: String) extends Opt[T] {
    val value: Value = lens optFrom strValue getOrElse errorValue
  }
  def newOpt[T: Lens](name: String, strValue: String): TOpt[T] = TOpt(name, strValue)

  class StringOption(name: String, strValue: String) extends TOpt[String](name, strValue) { }

  trait Mutable[T] extends Proptional[T] {
    private var _value: Value = zeroValue
    def value: Value = _value
    def value_=(v: Value): Unit = _value = v
    def strValue_=(str: String): Unit = lens optFrom str foreach (value = _)
    def typed_=(t: T): Unit = lens optInto typed foreach (value = _)
    def clear(): Unit = value = zeroValue
  }
  trait Prop[T] extends Proptional[T] {
    def propName: String
    def propSet: ProptionSet
    def value: Value    = propSet get propName flatMap lens.optFrom getOrElse zeroValue
    def unparse: String = "-D%s=%s".format(propName, strValue)
  }
  trait Opt[T] extends Proptional[T] {
  }
  trait Proption[T] extends Prop[T] with Opt[T] {
  }
  trait MutableProption[T] extends Proption[T] with Mutable[T] {
    override def value: Value = super[Mutable].value
  }
}