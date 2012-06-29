/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package cmd
package program

import nsc.io._

object Members {
  import scala.tools.nsc._
  import scala.collection.{ mutable, immutable, generic }
  import util._

  def main(args: Array[String]): Unit = {
    val settings = new Settings(println)
    val global = new Global(settings)
    val run = new global.Run()
    import global._

    val CollectionsPackageClass = definitions.getModule("scala.collection").tpe.typeSymbol

    val syms = args flatMap { path =>
      (path indexOf "scala/collection") match {
        case -1   => None
        case idx  =>
          val name = path drop idx stripSuffix ".scala" replace ('/', '.')
          try Some(definitions.getClass(name))
          catch { case _ => None }
      }
    }
    val allDecls = syms flatMap (_.tpe.decls.toList) filter (_.isMethod) toSet;
    val buf = new mutable.HashMap[Symbol, List[Symbol]] withDefaultValue Nil
    for {
      clazz <- syms
      member <- clazz.tpe.nonPrivateMembers
      if (member.isMethod)
      if (member.isPublic)
      if (!member.isFinal)
      if (syms forall (_ ne member.owner))
      if (member hasTransOwner CollectionsPackageClass)
    } {
      buf(clazz) = buf(clazz) :+ member
    }
    syms foreach { sym =>
      val missed = buf(sym) sortBy (x => (x.isDeprecated, x.name.toString))
      println(sym + " missed:")
      missed map { x =>
        val dep = if (x.isDeprecated) " (deprecated)" else ""
        "  " + x + " from " + x.owner + dep
      } foreach println
    }
  }
}

object ProxyCheck {
  import scala.collection.{ mutable, immutable }
  import immutable.NumericRange
  import reflect.{ Manifest, NameTransformer }
  import NameTransformer.decode
  import java.lang.reflect.{ Method, Modifier }

  def isPublic(m: Method)            = Modifier.isPublic(m.getModifiers)
  def isFinal(m: Method)             = Modifier.isFinal(m.getModifiers)
  def isSpecialization(m: Method)    = m.getName endsWith "$sp"
  def isSuperAccessor(m: Method)     = m.getName contains "super$"
  def shouldExclude(m: Method)       = isSpecialization(m) || isSuperAccessor(m)
  def methodsOf(clazz: Class[_])     = clazz.getMethods.toList filterNot shouldExclude
  def declMethodsOf(clazz: Class[_]) = clazz.getDeclaredMethods.toList filterNot shouldExclude

  def mapOnto[T, U](xs: Traversable[T], f: T => U) = Map(xs.toSeq map (x => (x -> f(x))): _*)
  def distinctNames(xs: Traversable[Method]) = (xs.toList map (x => decode(x.getName))).sorted.distinct
  implicit val methodOrdering: Ordering[Method] = Ordering[String] on (x => x.getName)

  class ManifestWrapper[T: Manifest] {
    def implClass(interface: Class[_]): Option[Class[_]] = {
      try Some(Class.forName(interface.getName + "$class"))
      catch { case x => None }
    }

    val clazz = manifest[T].erasure
    val clazzName = clazz.getName split '.' last;
    val baseClasses = {
      def loop(current: Class[_]): List[Class[_]] = {
        val xs = Option(current.getSuperclass).toList ++ current.getInterfaces
        xs ++ (xs flatMap loop)
      }
      loop(clazz).distinct
    }
    val (interfaces, superClasses) = baseClasses partition (_.isInterface)

    val implClassMap = mapOnto(interfaces, implClass)
    val implClasses  = implClassMap.values.toList.flatten
    val implMethods  = implClasses flatMap methodsOf

    val methodMap     = mapOnto(baseClasses, methodsOf)
    val methodList    = methodMap.values.toList.flatten.sorted
    val originMap     = methodList groupBy (_.getName) mapValues (xs => xs map (_.getDeclaringClass) distinct) toMap
    val methodNames   = distinctNames(methodList filterNot shouldExclude)

    def declaredMethods = declMethodsOf(clazz)
    override def toString = "ManifestWrapper for " + clazzName
  }

  case class Result(clazzName: String, intercepted: List[Method], missed: List[Method]) {
    def --(other: Result): Result = Result(
      clazzName + " (-- " + other.clazzName + ")",
      intercepted filterNot (other.intercepted contains _),
      missed filterNot (other.missed contains _)
    )

    override def toString = (
      "Result for " + clazzName + ":\n" +
      distinctNames(intercepted).mkString("  Intercepted: ", " ", "\n") +
      distinctNames(missed).mkString("  Missed: ", " ", "\n")
    )
  }

  class ProxyCheck[T: Manifest, U: Manifest]() {
    val underlying = new ManifestWrapper[T]
    val proxy      = new ManifestWrapper[U]

    val underlyingMs = underlying.methodList filter isOverridable
    val proxyMs      = proxy.declaredMethods filter isPublic

    def isOverridable(m: Method) = isPublic(m) && !isFinal(m)

    def matches(m1: Method, m2: Method) = {
      m1.getName == m2.getName &&
      m1.getTypeParameters.size == m2.getTypeParameters.size &&
      m1.getParameterTypes.toList == m2.getParameterTypes.toList &&
      (m1.getReturnType isAssignableFrom m2.getReturnType)
    }

    val (interceptedMethods, missedMethods) =
      underlyingMs partition { m1 =>
        proxyMs exists { m2 =>
          matches(m1, m2)
        }
      }

    def check() = Result(proxy.clazzName, interceptedMethods, missedMethods)
  }

  def checkAgainstSuperclass[T: Manifest]() = {
    val m1 = manifest[T]
    val m2 = Manifest.classType[Any](m1.erasure.getInterfaces.head: Class[_])
    val pc = new ProxyCheck()(m2, m1)
    val result = pc.check()
    println
    result
  }

  import Simple.{ SimpleReference, scalaProgramInfo }
  private val info        = scalaProgramInfo("proxyCheck", "Usage: proxyCheck")
  lazy val ProxyCheckSpec = new SimpleReference(info)

  def main(args0: Array[String]): Unit = {
    val runner = ProxyCheckSpec instance args0
    import runner._
    import collection._

    val r1 = checkAgainstSuperclass[TraversableProxyLike[_, _]]
    println(r1)
    val r2 = checkAgainstSuperclass[IterableProxyLike[_, _]] -- r1
    println(r2)

    new ProxyCheck[SeqLike[_, _], SeqProxyLike[_, _]] check();
    // checkAgainstSuperclass[SeqProxyLike[_, _]]
    checkAgainstSuperclass[MapProxyLike[_, _, _]]
    checkAgainstSuperclass[SetProxyLike[_, _]]

    ()
  }
}
