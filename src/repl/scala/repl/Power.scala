/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.repl

import scala.tools.nsc._
import scala.collection.{ mutable, immutable }
import scala.util.matching.Regex
import scala.reflect.internal.util.{ BatchSourceFile }
import session.{ History }
import scala.io.Codec
import java.net.URL
import io.{ Path }
import language.implicitConversions
import scala.reflect.{ api, base, runtime }
import scala.annotation.{ implicitWeight => weight }

trait TagWrappers {
  val global: api.Universe
  import global._

  class TaggedValue[T : TypeTag : ClassTag](val value: T) {
    def tag   = typeTag[T]
    def ctag  = classTag[T]

    override def toString = value match {
      case null => "" + typeOf[T]
      case x    => "%s: %s".format(x, typeOf[T])
    }
  }
  implicit def newTaggedValue[T : TypeTag : ClassTag](value: T): TaggedValue[T] = new TaggedValue[T](value)
}

trait ReplInternalInfos extends TagWrappers {
  val intp: scala.repl.IMain
  lazy val global: intp.global.type = intp.global
  import global._

  override implicit def newTaggedValue[T : TypeTag : ClassTag](value: T): TaggedValue[T] = new TaggedValue[T](value)

  class TaggedValue[T : TypeTag : ClassTag](value0: T) extends super.TaggedValue[T](value0) {
    def ? = newInfo(value)
  }

  def newInfo[T : TypeTag : ClassTag](value: T): InternalInfo = new InternalInfo(value)

  /** Todos...
   *    translate tag type arguments into applied types
   *    customizable symbol filter (had to hardcode no-spec to reduce noise)
   */
  class InternalInfo(taggedValue: TaggedValue[_]) {
    def tag          = taggedValue.tag
    def ctag         = taggedValue.ctag
    def tpe          = tag.tpe
    def symbol       = tpe.typeSymbol
    def runtimeClass = ctag.runtimeClass

    private def isSpecialized(s: Symbol) = s.name.toString contains "$mc"
    private def isImplClass(s: Symbol)   = s.name.toString endsWith "$class"

    /** Standard noise reduction filter. */
    def excludeMember(s: Symbol) = (
         isImplClass(s)
      || s.isAnonOrRefinementClass
      || s.isAnonymousFunction
    )
    def baseClasses  = tpe.baseClasses
    def baseTypes    = tpe.baseTypeSeq.toList
    def companion    = symbol.companionSymbol
    def declsByClass = mapFrom(baseClasses)(_.info.decls.toList.sortBy(_.name))
    def moduleClass  = symbol.moduleClass
    def name         = symbol.name
    def owner        = symbol.owner
    def owners       = symbol.ownerChain drop 1
    def parents      = tpe.parents
    def shortClass   = runtimeClass.getName split "[$.]" last
    def sig          = symbol.defString

    def <:<[U: TypeTag](other: U) = tpe <:< typeOf[U]
    def lub[U: TypeTag](other: U) = global.lub(List(tpe, typeOf[U]))
    def glb[U: TypeTag](other: U) = global.glb(List(tpe, typeOf[U]))

    override def toString = taggedValue match {
      case null   => runtimeClass.getName
      case x      => "%s (%s)".format(x, shortClass)
    }
  }
}

object Power {
  implicit class ReplInputStreamOps(in: InputStream)(implicit codec: Codec) {
    def bytes(): Array[Byte] = io.Streamable.bytes(in)
    def slurp(): String      = io.Streamable.slurp(in)
  }
  implicit class ReplUrlOps(val url: URL) extends AnyVal {
    def slurp(implicit codec: Codec): String = io.Streamable.slurp(url)
  }
}

/** A class for methods to be injected into the intp in power mode.
 *  The type "T" determines what will be imported directly into the
 *  repl: all the members of that type.
 */
class Power[T : TypeTag : ClassTag](val intp: IMain, vals: T) {
  import intp.{ beQuietDuring, typeOfExpression, interpret, parse }
  import intp.global._

  def banner = """
    |** Power User mode enabled - TICK TICK WHIR **
    |** It's using reflection now.               **
  """.stripMargin.trim

  private def initImports = List(
    "scala.repl._",
    // "scala.collection.JavaConverters._",
    "$r.replenv._"
    // "treedsl.CODE._"
  )
  def init = initImports.mkString("import ", ", ", "")

  /** Starts up power mode and runs whatever is in init.
   */
  def unleash(): Unit = beQuietDuring {
    // First we create the ReplVals instance and bind it to $r
    intp.bind[T]("$r", vals)
    // Then we import everything from $r, via its true path.
    // Later imports rely on the repl's name resolution to find $r.
    intp interpret ("import " + intp.pathToTerm("$r") + "._")
    // And whatever else there is to do.
    init.lines foreach (intp interpret _)
  }

  object StringPrettifier extends Prettifier[String] {
    def show(x: String) = println(x)
    def prettify(x: String) = List(Prettifier stringOf x)
  }
  object Prettifier {
    @weight(-1) implicit object AnyPrettifier extends Prettifier[Any] {
      def show(x: Any): Unit = prettify(x) foreach println
      def prettify(x: Any): IterableOnce[String] = x match {
        case x: Name                => List(x.decode)
        case Tuple2(k, v)           => List(prettify(k).toIterator ++ Iterator("->") ++ prettify(v) mkString " ")
        case xs: Array[_]           => xs.iterator flatMap prettify
        case xs: IterableOnce[_] => xs flatMap prettify
        case x                      => List(Prettifier.stringOf(x))
      }
    }

    def stringOf(x: Any): String = scala.runtime.ScalaRunTime.stringOf(x)
    def prettify[T](value: T): IterableOnce[String] = default[T] prettify value
    def default[T] = new Prettifier[T] {
      def prettify(x: T): IterableOnce[String] = AnyPrettifier prettify x
      def show(x: T): Unit = AnyPrettifier show x
    }
  }
  trait Prettifier[T] {
    def show(x: T): Unit
    def prettify(x: T): IterableOnce[String]

    def show(xs: IterableOnce[T]): Unit = prettify(xs) foreach println
    def prettify(xs: IterableOnce[T]): IterableOnce[String] = xs flatMap (x => prettify(x))
  }

  abstract class PrettifierClass[T: Prettifier]() {
    val pretty = implicitly[Prettifier[T]]
    import pretty._

    def value: Seq[T]

    def pp(f: Seq[T] => Seq[T]): Unit =
      pretty prettify f(value) foreach (StringPrettifier show _)

    def freq[U](p: T => U) = (value.toSeq groupBy p mapValues (_.size)).toList sortBy (-_._2) map (_.swap)
    def ppfreq[U](p: T => U): Unit = freq(p) foreach { case (count, key) => println("%5d %s".format(count, key)) }

    def |[U](f: Seq[T] => Seq[U]): Seq[U]        = f(value)
    def ^^[U](f: T => U): Seq[U]                 = value map f
    def ^?[U](pf: PartialFunction[T, U]): Seq[U] = value collect pf

    def >>!(implicit ord: Ordering[T]): Unit     = pp(_.sorted.distinct)
    def >>(implicit ord: Ordering[T]): Unit      = pp(_.sorted)
    def >!(): Unit                               = pp(_.distinct)
    def >(): Unit                                = pp(identity)

    def >#(): Unit                               = this ># (identity[T] _)
    def >#[U](p: T => U): Unit                   = this ppfreq p

    def >?(p: T => Boolean): Unit                = pp(_ filter p)
    def >?(s: String): Unit                      = pp(_ filter (_.toString contains s))
    def >?(r: Regex): Unit                       = pp(_ filter (_.toString matches fixRegex(r)))

    private def fixRegex(r: scala.util.matching.Regex): String = {
      val s = r.pattern.toString
      val prefix = if (s startsWith "^") "" else """^.*?"""
      val suffix = if (s endsWith "$") "" else """.*$"""

      prefix + s + suffix
    }
  }

  class MultiPrettifierClass[T: Prettifier](val value: Seq[T]) extends PrettifierClass[T]() { }
  class SinglePrettifierClass[T: Prettifier](single: T) extends PrettifierClass[T]() {
    val value = List(single)
  }

  trait Implicits {
    @weight(-2) implicit def replPrinting[T](x: T)(implicit pretty: Prettifier[T] = Prettifier.default[T]) =
      new SinglePrettifierClass[T](x)

    @weight(-1) implicit def replMultiPrinting[T: Prettifier](xs: IterableOnce[T]): MultiPrettifierClass[T] =
      new MultiPrettifierClass[T](xs.toSeq)
    @weight(-1) implicit def replPrettifier[T] : Prettifier[T] = Prettifier.default[T]
  }

  def context(code: String)  = analyzer.rootContext(unit(code))
  def source(code: String)   = newSourceFile(code)
  def unit(code: String)     = newCompilationUnit(code)
  def trees(code: String)    = parse(code) getOrElse Nil
  def seenTypeOf(id: String) = intp.typeOfExpression(id)
}
