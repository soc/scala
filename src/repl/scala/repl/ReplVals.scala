/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.repl

import language.implicitConversions
import scala.tools.nsc.ast.TreeDSL
import scala.reflect.internal.util.StringOps
import scala.reflect.{ ClassTag, classTag, api, base, runtime }
import scala.reflect.runtime.{ universe => ru }
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
  val global: scala.tools.nsc.Global
  import global._

  implicit final class ReplSymbolListOps(val syms: List[Symbol]) {
    def sigs: List[String] = syms map (_.defString)
    def infos: List[Type]  = syms map (_.info)
  }
}

/** A class which the repl utilizes to expose predefined objects.
 *  The base implementation is empty; the standard repl implementation
 *  is StdReplVals.
 */
abstract class ReplVals { }

class StdReplVals(final val r: ILoop) extends ReplVals {
  final lazy val repl                     = r
  final lazy val intp                     = r.intp
  final lazy val power                    = r.power
  final lazy val reader                   = r.in
  final lazy val vals                     = this
  final lazy val global: intp.global.type = intp.global
  final lazy val isettings                = intp.isettings
  final lazy val completion               = reader.completion
  final lazy val history                  = reader.history
  // final lazy val phased                   = power.phased
  final lazy val analyzer                 = global.analyzer

  final lazy val replenv = new {
    val global: intp.global.type = intp.global
  } with power.Implicits with ReplGlobalOps { }

  object treedsl extends { val global: intp.global.type = intp.global } with TreeDSL { }

  def lastRequest = intp.lastRequest

  //
  // final lazy val typer = analyzer.newTyper(
  //   analyzer.rootContext(
  //     power.unit("").asInstanceOf[analyzer.global.CompilationUnit]
  //   )
  // )
  //
  // class ReplImplicits extends power.Implicits {
  //   import intp.global._
  //
  //   private val tagFn = ReplVals.mkCompilerTypeFromTag[intp.global.type](global)
  //   implicit def mkCompilerTypeFromTag(sym: Symbol) = tagFn(sym)
  // }

  // def typed[T <: analyzer.global.Tree](tree: T): T = typer.typed(tree).asInstanceOf[T]
}
//
// object ReplVals {
//   /** Latest attempt to work around the challenge of foo.global.Type
//    *  not being seen as the same type as bar.global.Type even though
//    *  the globals are the same.  Dependent method types to the rescue.
//    */
//   def mkCompilerTypeFromTag[T <: Global](global: T) = {
//     import global._
//     import definitions._
//
//     /** We can't use definitions.compilerTypeFromTag directly because we're passing
//      *  it to map and the compiler refuses to perform eta expansion on a method
//      *  with a dependent return type.  (Can this be relaxed?) To get around this
//      *  I have this forwarder which widens the type and then cast the result back
//      *  to the dependent type.
//      */
//     def compilerTypeFromTag(t: base.Universe # AbsTypeTag[_]): Global#Type =
//       definitions.compilerTypeFromTag(t)
//
//     class AppliedTypeFromTags(sym: Symbol) {
//       def apply[M](implicit m1: ru.TypeTag[M]): Type =
//         if (sym eq NoSymbol) NoType
//         else appliedType(sym, compilerTypeFromTag(m1).asInstanceOf[Type])
//
//       def apply[M1, M2](implicit m1: ru.TypeTag[M1], m2: ru.TypeTag[M2]): Type =
//         if (sym eq NoSymbol) NoType
//         else appliedType(sym, compilerTypeFromTag(m1).asInstanceOf[Type], compilerTypeFromTag(m2).asInstanceOf[Type])
//     }
//
//     (sym: Symbol) => new AppliedTypeFromTags(sym)
//   }
// }
