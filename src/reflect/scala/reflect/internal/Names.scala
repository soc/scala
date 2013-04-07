/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import scala.io.Codec
import java.security.MessageDigest
import scala.language.implicitConversions
import scala.reflect.api.Universe
import scala.collection.{ mutable, immutable, generic }
import scala.reflect.NameTransformer._

trait Names extends api.Names {
  implicit class NameListOps(val segs: List[Name]) extends AnyRef with naming.ListOpsForNameLists[Name] {
    private def assumeTerm = segs match {
      case Nil     => true
      case hd :: _ => hd.isTermName
    }
    def newName(stringValue: String): Name = Name(stringValue, isTerm = assumeTerm)
    def mkName(sep: String): Name = Name(segs mkString sep, isTerm = assumeTerm)
  }
  implicit class TermNameListOps(val segs: List[TermName]) extends AnyRef with naming.ListOpsForNameLists[TermName] {
    def newName(stringValue: String): TermName = TermName(stringValue)
    def mkName(sep: String): TermName = TermName(segs mkString sep)
  }
  implicit class TypeNameListOps(val segs: List[TypeName]) extends AnyRef with naming.ListOpsForNameLists[TypeName] {
    def newName(stringValue: String): TypeName = TypeName(stringValue)
    def mkName(sep: String): TypeName = TypeName(segs mkString sep)
  }

  sealed abstract class Name extends AnyRef with naming.Name with naming.NameOfType[Name] {
    def companionName: Name

    def nameLogic = naming.StandardNameLogic
    def bothNames = List(toTermName, toTypeName)
    def start     = stringValue.##

    /** Replace operator symbols by corresponding \$op_name. */
    def encode = {
      val str = stringValue
      val res = NameTransformer.encode(str)
      if (res == str) thisName else newName(res)
    }

    /** Replace \$op_name by corresponding operator symbol. */
    def decode: String = {
      if (this containsChar '$') {
        val str = stringValue
        val res = NameTransformer.decode(str)
        if (res == str) str
        else res
      }
      else stringValue
    }

    def dropLocal   = thisName stripSuffix LOCAL_SUFFIX_STRING
    def dropModule  = thisName stripSuffix MODULE_SUFFIX_STRING
    def dropSetter  = thisName stripSuffix SETTER_SUFFIX_STRING
    def isLocal     = stringValue endsWith LOCAL_SUFFIX_STRING
    def isModule    = stringValue endsWith MODULE_SUFFIX_STRING
    def isSetter    = stringValue endsWith SETTER_SUFFIX_STRING
    def isImplClass = stringValue endsWith IMPLCLASS_SUFFIX_STRING
    def localName   = getterName append LOCAL_SUFFIX_STRING
    def moduleName  = toTermName.dropLocal append MODULE_SUFFIX_STRING
    def setterName  = toTermName.getterName append SETTER_SUFFIX_STRING
    def getterName  = dropTraitSetterSeparator.dropSetter.dropLocal.toTermName

    private def dropTraitSetterSeparator: TermName =
      stringValue indexOf TRAIT_SETTER_SEPARATOR_STRING match {
        case -1  => toTermName
        case idx => toTermName drop idx + TRAIT_SETTER_SEPARATOR_STRING.length
      }

    override def toTermName: TermName = thisName match {
      case t: TermName => t
      case _           => TermName(stringValue)
    }
    override def toTypeName: TypeName = thisName match {
      case t: TypeName => t
      case _           => TypeName(stringValue)
    }
    def isOperatorName = decoded != stringValue  // used by ide
    def debugString    = if (isTypeName) stringValue else stringValue + "!"
  }

  final class TermName private (val stringValue: String) extends Name with naming.TermName with naming.NameOfType[TermName] {
    override type ThisNameType = TermName
    def companionName: TypeName = toTypeName
    def thisName: TermName = this
    def newName(str: String): TermName = TermName(str)
  }
  final class TypeName private (val stringValue: String) extends Name with naming.TypeName with naming.NameOfType[TypeName] {
    override type ThisNameType = TypeName
    def companionName: TermName = toTermName
    def thisName: TypeName = this
    def newName(str: String): TypeName = TypeName(str)
  }

  implicit def liftApiTypeName(x: naming.TypeName): TypeName = TypeName(x.stringValue)
  implicit def liftApiTermName(x: naming.TermName): TermName = TermName(x.stringValue)

  object Name {
    def apply(s: String, isTerm: Boolean): Name = if (isTerm) TermName(s) else TypeName(s)
  }

  object TypeName extends TypeNameExtractor {
    private val cache = new java.util.HashMap[String, TypeName]()
    def apply(s: String): TypeName = {
      if (cache containsKey s) cache get s
      else {
        val v = new TypeName(s)
        cache.put(s, v)
        v
      }
    }
    def unapply(name: TypeName): Option[String] = Some(name.stringValue)
  }

  object TermName extends TermNameExtractor {
    private val cache = new java.util.HashMap[String, TermName]()
    def apply(s: String): TermName = {
      if (cache containsKey s) cache get s
      else {
        val v = new TermName(s)
        cache.put(s, v)
        v
      }
    }
    def unapply(name: TermName): Option[String] = Some(name.stringValue)
  }

  def newTermName(stringValue: String): TermName = TermName(stringValue)
  def newTypeName(stringValue: String): TypeName = TypeName(stringValue)

// Classes ----------------------------------------------------------------------

  implicit val NameTag = ClassTag[Name](classOf[Name])
  implicit val TypeNameTag = ClassTag[TypeName](classOf[TypeName])
  implicit val TermNameTag = ClassTag[TermName](classOf[TermName])
}
