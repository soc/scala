package scala.reflect
package api

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 * This trait defines `Name`s in Scala Reflection, and operations on them.
 *
 *  Names are simple wrappers for strings. [[scala.reflect.api.Names#Name Name]] has two subtypes
 *  [[scala.reflect.api.Names#TermName TermName]] and [[scala.reflect.api.Names#TypeName TypeName]]
 *  which distinguish names of terms (like objects or members) and types. A term and a type of the
 *  same name can co-exist in an object.
 *
 *  To search for the `map` method (which is a term) declared in the `List` class, one can do:
 *
 * {{{
 *   scala> typeOf[List[_]].member(newTermName("map"))
 *   res0: reflect.runtime.universe.Symbol = method map
 * }}}
 *
 *  To search for a type member, one can follow the same procedure, using `newTypeName` instead.
 *
 *  For more information about creating and using `Name`s, see the [[http://docs.scala-lang.org/overviews/reflection/annotations-names-scopes.html Reflection Guide: Annotations, Names, Scopes, and More]]
 *
 *  @contentDiagram hideNodes "*Api"
 *  @group ReflectionAPI
 */
trait Names {
  /** An implicit conversion from String to TermName.
   *  Enables an alternative notation `"map": TermName` as opposed to `newTermName("map")`.
   *  @group Names
   */
  implicit def stringToTermName(s: String): TermName = TermName(s)

  /** An implicit conversion from String to TypeName.
   *  Enables an alternative notation `"List": TypeName` as opposed to `newTypeName("List")`.
   *  @group Names
   */
  implicit def stringToTypeName(s: String): TypeName = TypeName(s)

  /** The abstract type of names.
   *  @group Names
   */
  type Name >: Null <: naming.Name

  /** The abstract type of names representing terms.
   *  @group Names
   */
  type TypeName >: Null <: Name with naming.TypeName

  /** The abstract type of names representing types.
   *  @group Names
   */
  type TermName >: Null <: Name with naming.TermName

  /** The constructor/extractor for `TermName` instances.
   *  @group Extractors
   */
  val TermName: TermNameExtractor

  /** An extractor class to create and pattern match with syntax `TermName(s)`.
   *  @group Extractors
   */
  abstract class TermNameExtractor {
    def apply(s: String): TermName
    def unapply(name: TermName): Option[String]
  }

  /** The constructor/extractor for `TypeName` instances.
   *  @group Extractors
   */
  val TypeName: TypeNameExtractor

  /** An extractor class to create and pattern match with syntax `TypeName(s)`.
   *  @group Extractors
   */
  abstract class TypeNameExtractor {
    def apply(s: String): TypeName
    def unapply(name: TypeName): Option[String]
  }

  @deprecated("Use TermName instead", "2.11.0")
  def newTermName(s: String): TermName
  @deprecated("Use TypeName instead", "2.11.0")
  def newTypeName(s: String): TypeName
}
