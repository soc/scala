package scala
package reflect
package naming

import scala.io.Codec
import java.security.MessageDigest
import scala.language.implicitConversions
import scala.collection.{ mutable, immutable, generic }
import NameTransformer._

final class NameKind(val kind: String) {
  override def toString = kind
}
object NameKind {
  final val Type = new NameKind("type")
  final val Term = new NameKind("term")
}
trait NameLogic extends Any {
  def jvmEncodeName(in: String): String
  def jvmDecodeName(in: String): String
}
trait StandardNameLogic extends Any with NameLogic {
  /** Replace operator symbols by corresponding \$op_name. */
  def jvmEncodeName(in: String) = NameTransformer encode in
  /** Replace \$op_name by corresponding operator symbol. */
  def jvmDecodeName(in: String) = if (in contains '$') NameTransformer decode in else in
}
object StandardNameLogic extends StandardNameLogic

trait ListOpsForNameLists[T <: Name] extends Any {
  def newName(stringValue: String): T
  def mkName(sep: String): T
  def mkName(sep: Char): T = mkName(sep.toString)
}

trait NameOfType[+SourceName <: Name] extends Any {
  type ThisNameType <: SourceName

  def stringValue: String
  def thisName: SourceName
  def newName(str: String): ThisNameType
  def nameKind: NameKind
  def nameLogic: NameLogic
  def toTermName: TermName
  def toTypeName: TypeName

  def isTermName  = nameKind eq NameKind.Term
  def isTypeName  = nameKind eq NameKind.Type

  def encoded: String = nameLogic jvmEncodeName stringValue
  def decoded: String = nameLogic jvmDecodeName stringValue
  def encodedName     = newName(encoded)
  def decodedName     = newName(decoded)

  def isSimple: Boolean = !containsChar('.')
  def simpleName        = if (isSimple) thisName else segments.last

  def charAt(idx: Int) = stringValue charAt idx
  def length   = stringValue.length
  def isEmpty  = stringValue == ""
  def nonEmpty = !isEmpty
  def segments(sep: Char): List[SourceName] = (stringValue split sep).toList map newName
  def segments: List[SourceName] = segments('.')

  def containsName(name: String): Boolean = stringValue contains name
  def containsName(name: Name): Boolean   = this containsName name.stringValue
  def mapName(f: String => String): SourceName = newName(f(stringValue))
  def subName(start: Int, end: Int): SourceName = newName(stringValue.substring(start, end))
  def indexWhere(p: Char => Boolean) = stringValue indexWhere p

  def string_==(that: Name): Boolean   = stringValue == that.stringValue
  def string_==(that: String): Boolean = stringValue == that

  def startChar: Char                   = stringValue charAt 0
  def endChar: Char                     = stringValue charAt length - 1
  def startsWith(char: Char): Boolean   = nonEmpty && startChar == char
  def startsWith(name: String): Boolean = stringValue startsWith name
  def startsWith(name: Name): Boolean   = stringValue startsWith name.stringValue
  def endsWith(char: Char): Boolean     = nonEmpty && endChar == char
  def endsWith(name: String): Boolean   = stringValue endsWith name
  def endsWith(name: Name): Boolean     = stringValue endsWith name.stringValue

  def indexOf(ch: Char)                 = stringValue indexOf ch
  def indexOf(ch: Char, fromIndex: Int) = stringValue.indexOf(ch, fromIndex)
  def indexOf(s: String)                = stringValue.indexOf(s)
  def lastIndexOf(ch: Char)             = stringValue lastIndexOf ch
  def lastIndexOf(s: String)            = stringValue lastIndexOf s
  def lastIndexOf(ch: Char, idx: Int)   = stringValue.lastIndexOf(ch, idx)
  def containsChar(ch: Char)            = (stringValue indexOf ch) >= 0

  def append(suffix: String): SourceName  = newName(stringValue + suffix)
  def append(ch: Char): SourceName        = append(ch.toString)
  def append(suffix: Name): SourceName    = append(suffix.stringValue)
  def prepend(prefix: String): SourceName = newName(prefix + stringValue)

  def longString        = s"$nameKind $decoded"
  override def toString = stringValue
}

trait Name extends Any with NameOfType[Name] {
  override def hashCode = stringValue.##
  override def equals(other: Any) = other match {
    case that: Name => (this.nameKind eq that.nameKind ) && (this.stringValue == that.stringValue)
    case _          => false
  }
}

trait TermName extends Any with Name with NameOfType[TermName] {
  type ThisNameType <: TermName
  def nameKind = NameKind.Term
}
trait TypeName extends Any with Name with NameOfType[TypeName] {
  type ThisNameType <: TypeName
  def nameKind = NameKind.Type
}
