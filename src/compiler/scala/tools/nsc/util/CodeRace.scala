/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package util

import scala.collection.{ mutable, immutable }

/** You want to see how different implementations of the same function
 *  fare against one another.  Call all the implementations on every call
 *  to the externally visible function.  While you're in there, optionally
 *  verify that all the functions return the same result.
 *
 *    val fn = CodeRace("my controversial function", "theirs" -> theirsImpl, "mine" -> mineImpl)
 *    def someFn(arg1: X, arg2: Y) = fn((arg1, arg2))
 *
 *  The results may or may not be meaningful, but they stand a chance.
 */
class CodeRace[-In, +Out](description: String, impls: Seq[(String, In => Out)]) extends (In => Out) {
  private var calls  = 0
  private val labels = impls.toList map (_._1)
  private val map    = impls.toMap
  private val nanos  = mutable.Map[String, Long]() withDefaultValue 0L
  private[this] var validator: (Out, Out) => Boolean = null

  // sigh, macro man strikes again
  private def microseconds: String = scala.io.Codec.fileEncodingCodec.name match {
    case "UTF-8"  => "µs"
    case _        => "us"
  }

  // print results on shutdown
  def setShutdownHook(): this.type = {
    scala.sys addShutdownHook {
      if (calls > 0)
        println(toString)
    }
    this
  }
  // throw exception if any two results return false from given comparator
  def setValidator(cmp: (Out, Out) => Boolean): this.type = {
    validator = cmp
    this
  }
  def setValidator(): this.type = setValidator(_ == _)

  // The shuffles are coarse defense against any optimization taking place.
  def apply(args: In): Out = {
    calls += 1
    val results = scala.util.Random.shuffle(labels) map { name =>
      val fn     = map(name)
      val start  = System.nanoTime
      val result = fn(args)
      val end    = System.nanoTime

      nanos(name) += (end - start)
      result
    }
    if (validator ne null) {
      var xs = results
      while (xs.nonEmpty) {
        val x1 :: tl = xs
        tl foreach (x2 => assert(validator(x1, x2), "differing results in " + description + ": " + results))
        xs = xs.tail
      }
    }
    scala.util.Random.shuffle(results).head
  }

  // My subjective idea about when to change units.
  private def prettyTime(nanos: Long) = (
    if (nanos < 1e5) "%.3f %s" format (nanos.toDouble / 1e3, microseconds)
    else if (nanos < 1e8) "%.3f ms" format (nanos.toDouble / 1e6)
    else "%.3f  s" format (nanos.toDouble / 1e9)
  )

  def addImpl[In1 <: In, Out1 >: Out](impl: (String, In1 => Out1)): CodeRace[In1, Out1] =
    new CodeRace(description, impls :+ impl)

  override def toString = (
    calls + " calls to " + description + ":\n" +
    labels.map(x => "  %s: %s".format(x, prettyTime(nanos(x)))).mkString("\n")
  )
}

object CodeRace {
  def apply[In, Out](description: String, impls: (String, In => Out)*): CodeRace[In, Out] =
    new CodeRace(description, impls.toList)
}
