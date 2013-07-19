package scala
package reflect
package io
package classpath

import scala.collection.mutable

final class PackageRep private (val name: String) extends AnyVal with Ordered[PackageRep] {
  def compare(that: PackageRep) = name compare that.name
  override def toString = name
}

object PackageRep {
  final val isCache = Debug.isCache
  if (isCache && Debug.isDebug)
    scala.sys addShutdownHook trace("PackageRep cache")(cache.stats())

  val NoPackageRep = new PackageRep("")
  def apply(path: String): PackageRep = (
    if (path eq null) NoPackageRep
    else path lastIndexOf '/' match {
      case -1    => NoPackageRep
      case until => if (isCache) cache.getOrElseUpdate(path, until) else create(path, until)
    }
  )

  // It's surprisingly expensive to naively obtain the package of a few tens
  // of thousands of class names. This cache has about a 98% hit rate and avoids
  // creating new Strings when possible. It hurts wall clock time, so it's
  // off at the moment.
  // TODO - investigate time/space tradeoff.
  private object cache {
    private var hits = 0
    private val cache = mutable.Map[Int, List[PackageRep]]() withDefaultValue Nil

    def size  = names.size
    def names = cache.values.flatten.map(_.name).toList.sorted
    def rate = "%.2f" format (100 * hits).toDouble / (hits + size)
    // Typical: PackageRep cache size 1110, 42943 hits
    def stats(): String = s"hit rate $rate% ($hits / ${ hits + size })"

    def matchesChar(pkg: Char, other: Char): Boolean = (
         (pkg == other)
      || (pkg == '.') && (other == '/')
    )
    def matches(pkg: String, other: String, until: Int): Boolean = {
      var i = 0
      var ok = true
      while (ok && i < until) {
        ok = ok && matchesChar(pkg charAt i, other charAt i)
        i += 1
      }
      i == until
    }
    def update(until: Int, rep: PackageRep): PackageRep = {
      cache(until) ::= rep
      rep
    }
    def getOrElseUpdate(path: String, until: Int): PackageRep = {
      def loop(pkgs: List[PackageRep]): PackageRep = pkgs match {
        case Nil     => update(until, create(path, until))
        case x :: xs => if (matches(x.name, path, until)) try x finally hits += 1 else loop(xs)
      }
      loop(cache(until))
    }
  }

  private def create(s: String, until: Int): PackageRep = {
    val sb = new StringBuilder(until)
    var i = 0
    while (i < until) {
      val ch = s charAt i
      if (ch != '/') sb append ch
      else if (i > 0 && i < until - 1) sb append '.'

      i += 1
    }
    new PackageRep(sb.toString)
  }
}
