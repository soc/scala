/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package util

import scala.collection.{ mutable, immutable }

/** Wraps a T => R, caching all results, counting hits,
 *  and optionally dumping statistics at shutdown.
 */
class CountingCache[T, R](label: String)(impl: T => R) extends (T => R) {
  private[this] val hits  = mutable.HashMap[T, Int]()
  private[this] val cache = mutable.HashMap[T, R]()

  def dumpAtShutdown(): this.type = {
    scala.sys addShutdownHook {
      println(summaryString)
      dump()
    }
    this
  }

  def dump() = hits.toList sortBy (-_._2) foreach println
  def summaryString = "%s: %d misses, %d hits.".format(label, cache.size, hits.values.sum)
  override def toString = summaryString

  def apply(x: T): R = {
    cache get x match {
      case Some(r)  => hits(x) += 1 ; r
      case _        => val r = impl(x) ; cache(x) = r ; hits(x) = 0 ; r
    }
  }
}
