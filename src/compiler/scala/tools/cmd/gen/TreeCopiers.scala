package scala.tools.cmd.gen

import scala.reflect._
import scala.reflect.runtime.universe._

// TODO - make this a general purpose library
object TreeCopiers {
  implicit private def SymbolOrdering[T <: api.Universe#Symbol]: Ordering[T] = Ordering[String] on (_.name.toString)

  // The symbols representing type members of T for p is true.
  def typesMatching[T: WeakTypeTag](p: Type => Boolean) = (
    weakTypeOf[T].members.toList
        filter (s => s.isType && p(s.asType.toType))
      sortWith (_.asType.toType <:< _.asType.toType)
  )
  // The type members of T which conform to U.
  def subtypes[T: WeakTypeTag, U: WeakTypeTag] = typesMatching[T](_ <:< weakTypeOf[U])
  // The abstract methods of T.
  def deferredMethods[T: WeakTypeTag] = weakTypeOf[T].members collect {
    case m: MethodSymbol if m.asInstanceOf[internal.SymbolTable#Symbol].isDeferred => m
  }

  def caseClassSymbols(xs: List[Symbol]) = xs collect { case s: ClassSymbol if s.isCaseClass => s } sorted
  def classClassSymbols(xs: List[Symbol]) = xs collect { case s: ClassSymbol if !s.isTrait => s }
  def caseFieldAccessors(x: ClassSymbol) = x.typeSignature.declarations find (s => s.isMethod && s.asMethod.isPrimaryConstructor) match {
    case Some(c: MethodSymbol) => c.paramss.head
    case _                     => Nil
  }

  def coreClasses[U: WeakTypeTag] = caseClassSymbols(subtypes[internal.SymbolTable, U])
  def coreTrees = coreClasses[internal.SymbolTable#TreeApi]
  def coreTypes = coreClasses[internal.SymbolTable#TypeApi]
  def coreSymbols = coreClasses[internal.SymbolTable#SymbolApi]
  def treeCopierMethods = deferredMethods[internal.SymbolTable#InternalTreeCopierOps].toList.sorted
}
import TreeCopiers._

abstract class GenMatch {
  def allDefDefs: String

  def tpe_s(s: Symbol): String = tpe_s(s.typeSignature)
  def tpe_s(tp: Type): String = tp match {
    case This(name)              => name.toString
    case TypeRef(pre, sym, Nil)  => sym.name.toString
    case TypeRef(pre, sym, args) => s"""${sym.name}[${args map tpe_s mkString ", "}]"""
    case tp                      => tp.toString
  }

  class GenDef(clazz: ClassSymbol) {
    val name       = clazz.name
    val params     = caseFieldAccessors(clazz)
    val copyMethod: MethodSymbol = treeCopierMethods find (_.name == name.toTermName) map (_.asMethod) orNull
    val paramNames = params.map(_.name.toString.capitalize)
    val paramNamesPlus = "tree" :: paramNames
    val paramTypes = copyMethod match {
      case null => "Tree" :: (params map tpe_s)
      case m    => m.paramss.flatten map tpe_s
    }
    val paramList  = paramNamesPlus zip paramTypes map { case (p, t) => s"$p: $t" } mkString ", "
    val bindings   = paramNames mkString ", "
    val copyArgs   = paramNamesPlus mkString ", "
  }
}

object GenLazy extends GenMatch {
  def allDefDefs = {
    val text = coreTrees map genDefDef mkString "\n"
    text.lines.toList map ("    " + _) mkString "\n"
  }
  def genDefDef(clazz: ClassSymbol) = {
    val gdef = new GenDef(clazz)
    import gdef._

    val case1 = s"t @ $name($bindings)"
    val case2 = "_" + (" " * (case1.length - 1))

    s"""|def $name($paramList): $name = tree match {
        |  case $case1 => t
        |  case $case2 => treeCopy.$name($copyArgs)
        |}""".stripMargin.trim
  }
  def main(args: Array[String]): Unit = {
    val text = (
      s"""|  // Generated code.
          |  class LazyTreeCopier extends InternalTreeCopierOps {
          |    val treeCopy: TreeCopier = newStrictTreeCopier
          |$allDefDefs
          |  }""".stripMargin
    )
    println(text)
  }
}

object GenStrict extends GenMatch {
  def allDefDefs = {
    val text = treeCopierMethods map genFromAbstract mkString "\n"
    text.lines.toList map ("    " + _) mkString "\n"
  }
  def genFromAbstract(m: MethodSymbol) = {
    val name = m.name
    val params = m.paramss.flatten drop 1
    val paramNames = params map (_.name.toString)
    val paramTypes = params map tpe_s
    val paramList  = (paramNames, paramTypes).zipped map (_ + ": " + _) mkString ", "

    s"def $name($paramList): $name = new $name($paramList).copyAttrs(tree)"
  }
  def main(args: Array[String]): Unit = {
    val text = (
      s"""|  // Generated code.
          |  class StrictTreeCopier extends InternalTreeCopierOps {
          |$allDefDefs
          |  }""".stripMargin
    )
    println(text)
  }
}

object Gen {
  def main(args: Array[String]): Unit = {
    GenLazy main args
    GenStrict main args
  }
}
