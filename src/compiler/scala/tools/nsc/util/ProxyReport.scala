/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package util

import scala.collection.{ mutable, immutable, generic }

/** A class for analyzing forwarding/proxy relationships.
 */
trait ProxyReport {
  val global: Global
  import global._
  import definitions._

  private object classes {
    def isIgnorable(sym: Symbol) = sym :: sym.allOverriddenSymbols exists { s =>
      ObjectClass isSubClass s.owner
    }
    def nonPrivateMethods(sym: Symbol) = {
      val methods = sym.initialize.tpe.nonPrivateMembers filter { x =>
        x.isMethod && !x.isConstructor && !x.isPrivate && !isIgnorable(x)
      }
      methods foreach (m => m.initialize.info.paramss.flatten foreach (_.initialize))
      methods
    }
    lazy val GlobalClass     = getRequiredClass(classOf[Global].getName)
    lazy val GenericClass    = getRequiredModule("scala.collection.generic").moduleClass
    lazy val CollectionClass = getRequiredModule("scala.collection").moduleClass

    def getType(name: String)    = getMember(GlobalClass, newTypeName(name))
    def getColl(name: String)    = getMember(CollectionClass, newTypeName(name))
    def getGeneric(name: String) = getMember(GenericClass, newTypeName(name))

    // the following operations + those in RewrappingTypeProxy are all operations
    // in class Type that are overridden in some subclass
    // Important to keep this up-to-date when new operations are added!
    def TypeClass             = getType("Type")
    def SimpleTypeProxy       = getType("SimpleTypeProxy")
    def RewrappingTypeProxy   = getType("RewrappingTypeProxy")
  }
  import classes._

  val wrappedHeader = """
/** With respect to %s, %s wraps:
 */
trait Wrapped {
  """.trim
  val unwrappedHeader = """
/** With respect to %s, %s does NOT wrap:
 */
trait Unwrapped {
    """.trim

  def wrapReport(underlying: Symbol, proxy: Symbol) = {
    val underlyingMs         = nonPrivateMethods(underlying)
    val proxyMs              = nonPrivateMethods(proxy) filterNot (_.owner == underlying)
    val (wrapped, unwrapped) = underlyingMs partition (m =>
      proxyMs exists (p =>
        (p.name == m.name) && {
          val self = proxy.thisType
          val memberTp = self.memberType(p)
          val parentTp = self.memberType(m)

          refChecks.overridesTypeInPrefix(memberTp, parentTp, self)
          //  || {
          //   // if (p.paramss.flatten.length == m.paramss.flatten.length)
          //   //   println("names equal, overridesType false:\n  " + ((p, m, memberTp, parentTp, self)) + "\n")
          //
          //   false
          // }
        }
      )
    )

    def show(xs: List[Symbol], template: String) = {
      val lines = xs.map(_.initialize.defString).sorted.map("  " + _ + "\n")
      lines.mkString(template.format(underlying, proxy) + "\n", "", "}")
    }

    show(wrapped, wrappedHeader) + "\n\n" + show(unwrapped, unwrappedHeader)
  }

  lazy val wrappers = List(
    TypeClass        -> SimpleTypeProxy,
    TypeClass        -> RewrappingTypeProxy
  )

  def generate(dir: io.Directory) = {
    /** A proxy for a type (identified by field `underlying`) that forwards most
     *  operations to it (for exceptions, see WrappingProxy, which forwards even more operations).
     *  every operation that is overridden for some kind of types should be forwarded.
     */
    for ((clazz, proxy) <- wrappers) {
      val text = wrapReport(clazz, proxy)
      val file = dir / (proxy.fullName + ".scala") toFile;

      file writeAll text
      println("Created " + file)
    }
  }
}

object ProxyReportRunner {
  class ProxyGlobal(s: Settings) extends Global(s) {
    object proxyReport extends {
      val global: ProxyGlobal.this.type = ProxyGlobal.this
    } with util.ProxyReport
  }

  def main(args: Array[String]): Unit = {
    if (args.isEmpty)
      return println("Give an output directory as argument.")

    val dir = io.Directory(args(0)).createDirectory()
    val s = new Settings()
    s.processArguments(args.toList.tail, true)
    val g = new ProxyGlobal(s)
    val run = new g.Run()
    g.afterTyper(g.proxyReport.generate(dir))
  }
}
