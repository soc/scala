/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import language.implicitConversions
import scala.tools.nsc.ast.TreeDSL
import scala.reflect.internal.util.StringOps
import scala.reflect.{ ClassTag, classTag, api, base, runtime }
import scala.reflect.runtime.{ universe => ru }
import java.net.URL

trait TagWrappers {
  val global: api.Universe
  import global._

  class TaggedValue[T : TypeTag : ClassTag](val value: T) {
    def ttag = typeTag[T]
    def ctag = classTag[T]

    override def toString = value match {
      case null => "" + typeOf[T]
      case x    => "%s: %s".format(x, typeOf[T])
    }
  }
  implicit def newTaggedValue[T : TypeTag : ClassTag](value: T): TaggedValue[T] = new TaggedValue[T](value)
}

trait ReplInternalInfos extends TagWrappers {
  val global: scala.tools.nsc.Global
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
    def ttag         = taggedValue.ttag
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
  final lazy val global: intp.global.type = intp.global

  final lazy val repl       = r
  final lazy val intp       = r.intp
  final lazy val power      = r.power
  final lazy val reader     = r.in
  final lazy val vals       = this
  final lazy val isettings  = intp.isettings
  final lazy val completion = reader.completion
  final lazy val history    = reader.history
  final lazy val phased     = power.phased
  final lazy val analyzer   = global.analyzer

  final lazy val replenv = new {
    val global: intp.global.type = intp.global
  } with power.Implicits2 with ReplGlobalOps { }

  object treedsl extends { val global: intp.global.type = intp.global } with TreeDSL { }
}
