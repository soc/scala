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
// import scala.annotation.{ implicitWeight => weight }

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
  def keymaps = jline.console.KeyMap.keyMaps.asScala

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

/** A class which the repl utilizes to expose predefined objects.
 */
class StdReplVals(val r: ILoop) {
  lazy val repl                     = r
  lazy val intp                     = r.intp
  lazy val reader                   = r.in
  lazy val vals                     = this
  lazy val global: intp.global.type = intp.global
  lazy val completion               = reader.completion
  lazy val history                  = reader.history
  lazy val analyzer                 = global.analyzer

  def context(code: String)  = analyzer.rootContext(unit(code))
  def source(code: String)   = global.newSourceFile(code)
  def unit(code: String)     = global.newCompilationUnit(code)
  def trees(code: String)    = intp parse code
  def seenTypeOf(id: String) = intp.typeOfExpression(id)

  class ReplEnv(val global: intp.global.type) extends ReplUniverseOps with TagWrappers with Prettifiers {
    @weight(-2) implicit def replPrinting[T](x: T)(implicit pretty: Prettifier[T] = Prettifier.default[T]) =
      new SinglePrettifierClass[T](x)

    @weight(-1) implicit def replMultiPrinting[T: Prettifier](xs: IterableOnce[T]): MultiPrettifierClass[T] =
      new MultiPrettifierClass[T](xs.toSeq)
    @weight(-1) implicit def replPrettifier[T] : Prettifier[T] = Prettifier.default[T]
  }

  lazy val replenv = new ReplEnv(global)

  object treedsl extends { val global: intp.global.type = intp.global } with TreeDSL { }

  def lastRequest = intp.lastRequest
}
