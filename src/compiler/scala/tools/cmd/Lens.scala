package scala.tools
package cmd

trait Lens[T] {
  abstract class AbsValue {
    def typed: T
    def strValue: String
  }
  case class Value(strValue: String, typed: T) extends AbsValue { }

  def zeroValue: Value
  lazy val Value(zeroString, zero) = zeroValue

  def read(x: String): T
  def readDefined(x: String): Boolean

  def show(x: T): String = "" + x
  def showDefined(x: T) = true

  val fromString: PartialFunction[String, T] = { case x if readDefined(x)  => read(x) }
  val intoString: PartialFunction[T, String] = { case x if showDefined(x)  => show(x) }

  val optFrom: String => Option[Value] = s => fromString lift s map (t => Value(s, t))
  val optInto: T => Option[Value]      = t => intoString lift t map (s => Value(s, t))
}

object Lens {
  implicit object Boolean extends Lens[scala.Boolean] {
    private val ts  = Set("yes", "on", "true", "t")
    private val fs  = Set("no", "off", "false", "f")
    private val tfs = ts ++ fs

    def zeroValue              = Value("", false)
    def read(s: String)        = ts(s.toLowerCase)
    def readDefined(s: String) = tfs(s.toLowerCase)
  }
  implicit object String extends Lens[java.lang.String] {
    def zeroValue              = Value("", "")
    def read(s: String)        = s
    def readDefined(s: String) = true
  }
}