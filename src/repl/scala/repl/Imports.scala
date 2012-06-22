/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.repl

import scala.collection.{ mutable, immutable }

trait Imports {
  self: IMain =>

  import global._
  import definitions.{ ScalaPackage, JavaLangPackage, PredefModule }
  import memberHandlers._

  def isNoImports = settings.noimports.value
  def isNoPredef  = settings.nopredef.value

  /** Synthetic import handlers for the language defined imports. */
  private def makeWildcardImportHandler(sym: Symbol): ImportHandler = {
    val hd :: tl = sym.fullName.split('.').toList map newTermName
    val tree = Import(
      tl.foldLeft(Ident(hd): Tree)((x, y) => Select(x, y)),
      List(ImportSelector(nme.WILDCARD, -1, null, -1))
    )
    tree setSymbol sym
    new ImportHandler(tree)
  }

  /** Symbols whose contents are language-defined to be imported. */
  def languageWildcardSyms: List[Symbol] = List(JavaLangPackage, ScalaPackage, PredefModule)
  def languageWildcards: List[Type]      = languageWildcardSyms map (_.tpe)
  def languageWildcardHandlers           = languageWildcardSyms map makeWildcardImportHandler
  def isInDefaultNamespace(name: Name)   = languageWildcards exists (_.nonPrivateMember(name) != NoSymbol)

  def allImportedNames = importHandlers flatMap (_.importedNames)
  def importedTerms    = onlyTerms(allImportedNames)
  def importedTypes    = onlyTypes(allImportedNames)

  /** Types which have been wildcard imported, such as:
   *    val x = "abc" ; import x._  // type java.lang.String
   *    import java.lang.String._   // object java.lang.String
   *
   *  Used by tab completion.
   *
   *  XXX right now this gets import x._ and import java.lang.String._,
   *  but doesn't figure out import String._.  There's a lot of ad hoc
   *  scope twiddling which should be swept away in favor of digging
   *  into the compiler scopes.
   */
  def sessionWildcards: List[Type] = {
    importHandlers filter (_.importsWildcard) map (_.targetType) distinct
  }
  def wildcardTypes = languageWildcards ++ sessionWildcards

  def languageSymbols        = languageWildcardSyms flatMap membersAtPickler
  def sessionImportedSymbols = importHandlers flatMap (_.importedSymbols)
  def importedSymbols        = languageSymbols ++ sessionImportedSymbols
  def importedTermSymbols    = importedSymbols collect { case x: TermSymbol => x }
  def importedTypeSymbols    = importedSymbols collect { case x: TypeSymbol => x }
  def implicitSymbols        = importedSymbols filter (_.isImplicit)

  def importedTermNamed(name: String): Symbol =
    importedTermSymbols find (_.name.toString == name) getOrElse NoSymbol

  /** Tuples of (source, imported symbols) in the order they were imported.
   */
  def importedSymbolsBySource: List[(Symbol, List[Symbol])] = {
    val lang    = languageWildcardSyms map (sym => (sym, membersAtPickler(sym)))
    val session = importHandlers filter (_.targetType != NoType) map { mh =>
      (mh.targetType.typeSymbol, mh.importedSymbols)
    }

    lang ++ session
  }
  def implicitSymbolsBySource: List[(Symbol, List[Symbol])] = {
    importedSymbolsBySource map {
      case (k, vs) => (k, vs filter (_.isImplicit))
    } filterNot (_._2.isEmpty)
  }

  private case class RHS(req: Request, handler: MemberHandler, sym: Symbol) {
    def prefix = handler match {
      case x: ImportHandler => "" + x.expr
      case _                => req.prefix
    }
    override def toString = handler.exportedName(req.prefix, sym.name)
  }

  def importsCode(wanted: Set[Name]): List[String] = afterTyper {
    val importMap    = mutable.Map[Name, List[RHS]]() withDefaultValue Nil
    val allImplicits = mutable.ListBuffer[RHS]()

    for ((req, handler) <- allReqAndHandlers) {
      intp.pathToName(
      
      handler.exposedSymbols foreach (sym => importMap(sym.name) ::= RHS(req, handler, sym))
      allImplicits ++= (handler.implicitSymbols map (sym => RHS(req, handler, sym)))
    }
    val imps1 = wanted.toList map (name =>
      
      
      intp pathToName _)
    //   importMap(name).reverse.map(_.sym) match {
    //     case Nil => None
    //     case xs  => Some(importMap(name).head.prefix :: xs.map(_.name) mkString ".")
    //   }
    // }
    
    // val imps1 = wanted.toList flatMap { name =>
    //   importMap(name).reverse.map(_.sym) match {
    //     case Nil => None
    //     case xs  => Some(importMap(name).head.prefix :: xs.map(_.name) mkString ".")
    //   }
    // }
    val imps2 = allImplicits.toList.distinct filter (importMap.values.toSet contains _)

    println("allImplicits: " + allImplicits.mkString("\n  "))
    imps1 ++ imps2 map ("import " + _)
  }

  private def allReqAndHandlers =
    prevRequestList flatMap (req => req.handlers map (req -> _))

  private def membersAtPickler(sym: Symbol): List[Symbol] =
    beforePickler(sym.info.nonPrivateMembers)
}