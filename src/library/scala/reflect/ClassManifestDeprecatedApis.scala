/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package reflect

import scala.collection.mutable.{ WrappedArray, ArrayBuilder }
import java.lang.{ Class => jClass }

/** `ClassManifestFactory` defines factory methods for manifests.
 *  It is intended for use by the compiler and should not be used in client code.
 *
 *  Unlike `ClassManifest`, this factory isn't annotated with a deprecation warning.
 *  This is done to prevent avalanches of deprecation warnings in the code that calls methods with manifests.
 *
 *  In a perfect world, we would just remove the @deprecated annotation from `ClassManifest` the object
 *  and then delete it in 2.11. After all, that object is explicitly marked as internal, so noone should use it.
 *  However a lot of existing libraries disregarded the scaladoc that comes with `ClassManifest`,
 *  so we need to somehow nudge them into migrating prior to removing stuff out of the blue.
 *  Hence we've introduced this design decision as the lesser of two evils.
 */
object ClassManifestFactory {
  val Byte    = ManifestFactory.Byte
  val Short   = ManifestFactory.Short
  val Char    = ManifestFactory.Char
  val Int     = ManifestFactory.Int
  val Long    = ManifestFactory.Long
  val Float   = ManifestFactory.Float
  val Double  = ManifestFactory.Double
  val Boolean = ManifestFactory.Boolean
  val Unit    = ManifestFactory.Unit
  val Any     = ManifestFactory.Any
  val Object  = ManifestFactory.Object
  val AnyVal  = ManifestFactory.AnyVal
  val Nothing = ManifestFactory.Nothing
  val Null    = ManifestFactory.Null

  def fromClass[T](clazz: jClass[T]): ClassManifest[T] = clazz match {
    case java.lang.Byte.TYPE      => Byte.asInstanceOf[ClassManifest[T]]
    case java.lang.Short.TYPE     => Short.asInstanceOf[ClassManifest[T]]
    case java.lang.Character.TYPE => Char.asInstanceOf[ClassManifest[T]]
    case java.lang.Integer.TYPE   => Int.asInstanceOf[ClassManifest[T]]
    case java.lang.Long.TYPE      => Long.asInstanceOf[ClassManifest[T]]
    case java.lang.Float.TYPE     => Float.asInstanceOf[ClassManifest[T]]
    case java.lang.Double.TYPE    => Double.asInstanceOf[ClassManifest[T]]
    case java.lang.Boolean.TYPE   => Boolean.asInstanceOf[ClassManifest[T]]
    case java.lang.Void.TYPE      => Unit.asInstanceOf[ClassManifest[T]]
    case _                        => classType[T with AnyRef](clazz).asInstanceOf[ClassManifest[T]]
  }

  def singleType[T <: AnyRef](value: AnyRef): Manifest[T] = Manifest.singleType(value)

  /** ClassManifest for the class type `clazz`, where `clazz` is
    * a top-level or static class.
    * @note This no-prefix, no-arguments case is separate because
    *       it's called from ScalaRunTime.boxArray itself. If we
    *       pass varargs as arrays into this, we get an infinitely recursive call
    *       to boxArray. (Besides, having a separate case is more efficient)
    */
  def classType[T](clazz: jClass[_]): ClassManifest[T] =
    new ClassTypeManifest[T](None, clazz, Nil)

  /** ClassManifest for the class type `clazz[args]`, where `clazz` is
    * a top-level or static class and `args` are its type arguments */
  def classType[T](clazz: jClass[_], arg1: OptManifest[_], args: OptManifest[_]*): ClassManifest[T] =
    new ClassTypeManifest[T](None, clazz, arg1 :: args.toList)

  /** ClassManifest for the class type `clazz[args]`, where `clazz` is
    * a class with non-package prefix type `prefix` and type arguments `args`.
    */
  def classType[T](prefix: OptManifest[_], clazz: jClass[_], args: OptManifest[_]*): ClassManifest[T] =
    new ClassTypeManifest[T](Some(prefix), clazz, args.toList)

  def arrayType[T](arg: OptManifest[_]): ClassManifest[Array[T]] = arg match {
    case NoManifest          => Object.asInstanceOf[ClassManifest[Array[T]]]
    case m: ClassManifest[_] => m.asInstanceOf[ClassManifest[T]].arrayManifest
  }

  /** ClassManifest for the abstract type `prefix # name`. `upperBound` is not
    * strictly necessary as it could be obtained by reflection. It was
    * added so that erasure can be calculated without reflection. */
  def abstractType[T](prefix: OptManifest[_], name: String, clazz: jClass[_], args: OptManifest[_]*): ClassManifest[T] =
    new ClassManifest[T] {
      override def runtimeClass = clazz
      override val typeArguments = args.toList
      override def toString = prefix.toString+"#"+name+argString
    }

  /** ClassManifest for the abstract type `prefix # name`. `upperBound` is not
    * strictly necessary as it could be obtained by reflection. It was
    * added so that erasure can be calculated without reflection.
    * todo: remove after next boostrap
    */
  def abstractType[T](prefix: OptManifest[_], name: String, upperbound: ClassManifest[_], args: OptManifest[_]*): ClassManifest[T] =
    new ClassManifest[T] {
      override def runtimeClass = upperbound.runtimeClass
      override val typeArguments = args.toList
      override def toString = prefix.toString+"#"+name+argString
    }
}
