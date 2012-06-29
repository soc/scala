package scala.tools
package util

import scala.collection.{ mutable, immutable }
import java.util.{ Properties => JProperties }

// java.version   Java Runtime Environment version
// java.vendor  Java Runtime Environment vendor
// java.vendor.url  Java vendor URL
// java.home  Java installation directory
// java.vm.specification.version  Java Virtual Machine specification version
// java.vm.specification.vendor   Java Virtual Machine specification vendor
// java.vm.specification.name   Java Virtual Machine specification name
// java.vm.version  Java Virtual Machine implementation version
// java.vm.vendor   Java Virtual Machine implementation vendor
// java.vm.name   Java Virtual Machine implementation name
// java.specification.version   Java Runtime Environment specification version
// java.specification.vendor  Java Runtime Environment specification vendor
// java.specification.name  Java Runtime Environment specification name
// java.class.version   Java class format version number
// java.class.path  Java class path
// java.library.path  List of paths to search when loading libraries
// java.io.tmpdir   Default temp file path
// java.compiler  Name of JIT compiler to use
// java.ext.dirs  Path of extension directory or directories
// os.name  Operating system name
// os.arch  Operating system architecture
// os.version   Operating system version
// file.separator   File separator ("/" on UNIX)
// path.separator   Path separator (":" on UNIX)
// line.separator   Line separator ("\n" on UNIX)
// user.name  User's account name
// user.home  User's home directory
// user.dir   User's current working directory
//
object PropertySet {
  def stringConverter[T](implicit m: Manifest[T], fn: String => T = null): String => T = {
    import scala.reflect.Manifest._

    val f =
      if (m == Int) (x: String) => x.toInt
      else if (m == Long) (x: String) => x.toLong
      else if (m == Boolean) (x: String) => Set("yes", "on", "true")(x.toLowerCase)
      else if (m == manifest[String]) identity[String] _
      else if (fn != null) fn
      else (x: String) => error("No fromString implementation for '%s'" format m)

    f.asInstanceOf[String => T]
  }

  def sample(): PropertySet = {
    new PropertySet(System.getProperties()) {
      def partestActors = make[Int]("partest.actors")
      def partestErrors = make[Int]("partest.errors")
      def javaClassVersion = make[(Int, Int)](
        "java.class.version",
        (x: String) => {
          val xs = x split '.' map (_.toInt)
          (xs(0), xs(1))
        }
      )
      private def xs = List(partestActors, partestErrors, javaClassVersion)
      override lazy val knownMap = Map(xs map (x => x.propName -> x): _*)
    }
  }
  def fromJava(p: JProperties): PropertySet = new PropertySet(p)
}

class PropertySet(source: => JProperties) {
  private var underlying: JProperties = _
  private val allProps = mutable.Map[String, Mutable[_]]()
  private def readProps(): Unit = {
    import scala.collection.JavaConverters._
    allProps.clear()
    underlying = source
    require(underlying != null)
    underlying.asScala foreach { case (k, v) =>
      allProps += (k -> knownMap.getOrElse(k, makeAndSetString(k, v)))
    }
  }
  def refresh(): Unit = readProps()

  trait Readable[T] {
    val m: Manifest[T]

    /** The name of the property, for instance "user.home".
     */
    def propName: String

    /** Conversions between String and T.  A reasonable default is
     *  given in the easy direction, the other is abstract.
     */
    def fromString(x: String): T
    def intoString(x: T): String = "" + x

    /** The current value if the property is set, None otherwise.
     */
    def propValue: Option[T] = Option(underlying.getProperty(propName)) map fromString

    /** Validate that this String can represent type T.  Default
     *  implementation tries to convert and fails on exception.
     */
    def validate(x: String): Boolean =
      try validateType(fromString(x))
      catch { case _: Exception => false }

    def validateType(x: T): Boolean = true

    def unparse = propValue match {
      case Some(v)  => "-D%s=%s".format(propName, intoString(v))
      case _        => ""
    }

    def foreach[U](f: T => U): Unit = propValue foreach f
    def map[U](f: T => U): Option[U] = propValue map f
    def flatMap[U](f: T => Option[U]): Option[U] = propValue flatMap f
    def orElse(alt: => T) = propValue getOrElse alt

    override def toString = "%s: %s = %s".format(propName, m, propValue getOrElse "_")
  }
  trait Mutable[T] extends Readable[T] {
    def propString_=(str: String) {
      if (validate(str)) underlying.setProperty(propName, str)
      else validationError(propName, str)
    }
    def propValue_=(x: T): Unit = underlying.setProperty(propName, intoString(x))
    def clear() = underlying.remove(propName)
  }
  case class BooleanProp(propName: String) extends Mutable[Boolean] {
    val m = manifest[Boolean]
    def fromString(x: String) = List("yes", "on", "true") contains x.toLowerCase
  }
  case class StrictBooleanProp(propName: String) extends Mutable[Boolean] {
    val m = manifest[Boolean]
    def fromString(x: String) = x == "true"
    override def validate(x: String) = x == "true" || x == "false"
  }
  case class BoundedIntProp(propName: String, min: Int, max: Int) extends Mutable[Int] {
    val m = manifest[Int]
    def fromString(x: String) = x.toInt
    override def validateType(num: Int) = min <= num && num <= max
  }
  case class IntProp(propName: String) extends Mutable[Int] {
    val m = manifest[Int]
    def fromString(x: String) = x.toInt
  }
  case class LongProp(propName: String) extends Mutable[Long] {
    val m = manifest[Long]
    def fromString(x: String) = x.toLong
  }
  case class StringProp(propName: String) extends Mutable[String] {
    val m = manifest[String]
    def fromString(x: String) = x
  }

  def errorFn(msg: String): Unit = println(msg)
  def validationError(key: String, value: Any) = errorFn("Invalid value: '%s' cannot hold '%s'.".format(key, value))
  def knownMap: immutable.Map[String, Mutable[_]] = Map()

  def make[T: Manifest](name: String, f1: String => T, f2: T => String = null): Mutable[T] = new Mutable[T] {
    val m = manifest[T]
    val propName = name
    def fromString(x: String) = f1(x)
    override def intoString(x: T): String = if (f2 == null) super.intoString(x) else f2(x)
  }
  def make[T: Manifest](name: String): Mutable[_] = {
    import scala.reflect.Manifest._
    val m = manifest[T]

    if (m <:< Int) IntProp(name)
    else if (m <:< Long) LongProp(name)
    else if (m <:< Boolean) BooleanProp(name)
    else StringProp(name)
  }

  def makeAndSetString(name: String, value: String): StringProp = {
    val prop = StringProp(name)
    prop.propValue = value
    prop
  }

  def makeAndSet[T](name: String, value: T)(implicit m: Manifest[T], g: String => T = null): Mutable[T] = {
    val f: String => T = g match {
      case null => _ => error("Don't know how to make type '%s'" format m)
      case fn   => fn
    }
    val prop = make[T](name, f)
    prop.propValue = value
    prop
  }

  def apply[T: Manifest](name: String) = allProps(name)
  def get[T: Manifest](s: String) = Option(apply[T](s))
  def update[T: Manifest](key: String, value: T) {
    val prop = allProps.getOrElseUpdate(key, makeAndSet[T](key, value))
    if (manifest[T] <:< prop.m)
      (prop.asInstanceOf[Mutable[T]]).propValue = value
    else
      validationError(key, value)
  }

  def set(key: String, value: String) = allProps.getOrElseUpdate(key, makeAndSetString(key, value))
  def foreach[U](f: Mutable[_] => U): Unit = iterator foreach f
  def iterator: Iterator[Mutable[_]] = allProps.valuesIterator

  override def toString() = {
    val xs = iterator map ("  " + _ + "\n")
    xs.mkString("PropertySet(\n", "", ")\n")
  }
}