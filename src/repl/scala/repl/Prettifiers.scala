/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.repl

// import scala.tools.nsc._
// import scala.collection.{ mutable, immutable }
import scala.util.matching.Regex
// import scala.reflect.internal.util.{ BatchSourceFile }
// import session.{ History }
// import scala.io.Codec
// import java.net.URL
// import io.{ Path }
// import language.implicitConversions
import scala.reflect.{ api, base, runtime }
// import scala.annotation.{ implicitWeight => weight }
import scala.runtime.ScalaRunTime

trait Prettifiers {
  val global: api.Universe
  import global._

  object StringPrettifier extends Prettifier[String] {
    def show(x: String) = println(x)
    def prettify(x: String) = List(Prettifier stringOf x)
  }
  object Prettifier {
    @weight(-1) implicit object AnyPrettifier extends Prettifier[Any] {
      def show(x: Any): Unit = prettify(x) foreach println
      def prettify(x: Any): IterableOnce[String] = x match {
        case x: Name                => List(x.decoded)
        case Tuple2(k, v)           => List(prettify(k).toIterator ++ Iterator("->") ++ prettify(v) mkString " ")
        case xs: Array[_]           => xs.iterator flatMap prettify
        case xs: IterableOnce[_] => xs flatMap prettify
        case x                      => List(Prettifier.stringOf(x))
      }
    }

    def stringOf(x: Any): String = ScalaRunTime.stringOf(x)
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
}
