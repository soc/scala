/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.repl

import language.implicitConversions
import scala.tools.nsc.ast.TreeDSL
import scala.reflect.internal.util.StringOps
import scala.reflect.{ api, base, runtime }
import java.net.URL

trait ReplUniverseOps {
  val global: api.Universe
  import global._

  def moduleSym[T: TypeTag] = typeOf[T].typeSymbol match {
    case sym if sym.isTerm  => sym
    case sym                => sym.companionSymbol suchThat (_.isModule)
  }
  def classSym[T: TypeTag] = typeOf[T].typeSymbol suchThat (_.isClass)

  def lub[T1: TypeTag, T2: TypeTag] : Type              = global.lub(List(typeOf[T1], typeOf[T2]))
  def lub[T1: TypeTag, T2: TypeTag, T3: TypeTag] : Type = global.lub(List(typeOf[T1], typeOf[T2], typeOf[T3]))
  def glb[T1: TypeTag, T2: TypeTag] : Type              = global.glb(List(typeOf[T1], typeOf[T2]))
  def glb[T1: TypeTag, T2: TypeTag, T3: TypeTag] : Type = global.glb(List(typeOf[T1], typeOf[T2], typeOf[T3]))

  def toolbox = scala.tools.reflect.ToolBox(scala.reflect.runtime.currentMirror)

  implicit class ReplStringOps(val s: String) {
    def t: TypeName = newTypeName(s)
    def v: TermName = newTermName(s)

    // make an url out of the string
    def url: URL = (
      if (s contains ":") new URL(s)
      else if (new JFile(s) exists) new JFile(s).toURI.toURL
      else new URL("http://" + s)
    )
    def sanitize: String = StringOps.sanitize(s)
  }

  implicit class ReplSymbolOps(val sym: Symbol) {
    def tcon = sym.asTypeSymbol.asTypeConstructor

    def apply[T1: TypeTag] : Type =
      if (sym eq NoSymbol) NoType
      else appliedType(tcon, List(typeOf[T1]))

    def apply[T1: TypeTag, T2: TypeTag] : Type =
      if (sym eq NoSymbol) NoType
      else appliedType(tcon, List(typeOf[T1], typeOf[T2]))
  }

  implicit class ReplTypeListOps(val tps: List[Type]) {
    def lub: Type = global.lub(tps)
    def glb: Type = global.glb(tps)
  }
}

trait ReplGlobalOps extends ReplUniverseOps with ReplInternalInfos {
  val intp: scala.repl.IMain
  import global._

  implicit final class ReplSymbolListOps(val syms: List[Symbol]) {
    def sigs: List[String] = syms map (_.defString)
    def infos: List[Type]  = syms map (_.info)
  }
}

/** A class which the repl utilizes to expose predefined objects.
 */
class StdReplVals(final val r: ILoop) {
  final lazy val repl                     = r
  final lazy val intp                     = r.intp
  final lazy val power                    = r.power
  final lazy val reader                   = r.in
  final lazy val vals                     = this
  final lazy val global: intp.global.type = intp.global
  final lazy val completion               = reader.completion
  final lazy val history                  = reader.history
  final lazy val analyzer                 = global.analyzer

  final lazy val replenv = new {
    val intp: StdReplVals.this.intp.type = StdReplVals.this.intp
  } with power.Implicits with ReplGlobalOps { }

  object treedsl extends { val global: intp.global.type = intp.global } with TreeDSL { }

  def lastRequest = intp.lastRequest
}
