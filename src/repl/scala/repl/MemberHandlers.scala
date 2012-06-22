/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.repl

import scala.collection.{ mutable, immutable }
import scala.PartialFunction.cond
import scala.reflect.internal.Chars
import language.implicitConversions

trait MemberHandlers {
  val intp: IMain

  import intp.{ Request, global, naming }
  import global._
  import naming._

  def importableMembers(tp: Type) = (
    tp.nonPrivateMembers filterNot (sym =>
         sym.isConstructor
      || (definitions.ObjectClass isSubClass sym.owner)
    )
  )

  private def codegenln(leadingPlus: Boolean, xs: String*): String = codegen(leadingPlus, (xs ++ Array("\n")): _*)
  private def codegenln(xs: String*): String = codegenln(true, xs: _*)
  private def codegen(leadingPlus: Boolean, xs: String*): String = {
    val front = if (leadingPlus) "+ " else ""
    front + (xs map string2codeQuoted mkString " + ")
  }
  private implicit def name2string(name: Name) = name.toString

  /** A traverser that finds all mentioned identifiers, i.e. things
   *  that need to be imported.  It might return extra names.
   */
  private class ImportVarsTraverser extends Traverser {
    val importVars = new mutable.HashSet[Name]()

    override def traverse(ast: Tree) = ast match {
      case Ident(name) =>
        // XXX this is obviously inadequate but it's going to require some effort
        // to get right.
        if (name.toString startsWith "x$") ()
        else importVars += name
      case _        => super.traverse(ast)
    }
  }
  private object ImportVarsTraverser {
    def apply(member: Tree) = {
      val ivt = new ImportVarsTraverser()
      ivt traverse member
      ivt.importVars.toList
    }
  }

  def chooseHandler(member: Tree): MemberHandler = member match {
    case member: DefDef        => new DefHandler(member)
    case member: ValDef        => new ValHandler(member)
    case member: Assign        => new AssignHandler(member)
    case member: ModuleDef     => new ModuleHandler(member)
    case member: ClassDef      => new ClassHandler(member)
    case member: TypeDef       => new TypeAliasHandler(member)
    case member: Import        => new ImportHandler(member)
    case member                => new GenericHandler(member)
  }

  sealed abstract class MemberDefHandler(override val member: MemberDef) extends MemberHandler(member) {
    def symbol          = if (member.symbol eq null) NoSymbol else member.symbol
    def name: Name      = member.name
    def mods: Modifiers = member.mods
    def keyword         = member.keyword
    def prettyName      = name.decoded

    override def definesImplicit = mods.isImplicit
    override def definesTerm: Option[TermName] = Some(name.toTermName) filter (_ => name.isTermName)
    override def definesType: Option[TypeName] = Some(name.toTypeName) filter (_ => name.isTypeName)
    override def definedSymbols = if (symbol eq NoSymbol) Nil else List(symbol)
  }

  /** Class to handle one member among all the members included
   *  in a single interpreter request.
   */
  sealed abstract class MemberHandler(val member: Tree) {
    def definesImplicit = false
    def definesValue    = false
    def isLegalTopLevel = false

    def definesTerm     = Option.empty[TermName]
    def definesType     = Option.empty[TypeName]

    lazy val referencedNames = ImportVarsTraverser(member)
    def definedNames         = definesTerm.toList ++ definesType.toList
    def importedNames        = List[Name]()
    def exposedNames         = definedNames ++ importedNames
    def definedSymbols       = List[Symbol]()
    def importedSymbols      = List[Symbol]()
    def exposedSymbols       = definedSymbols ++ importedSymbols
    def implicitSymbols      = exposedSymbols filter (_.isImplicit)

    def exposesName(name: Name): Boolean = exposedNames contains name
    def exposesName(name: String): Boolean = (
      exposesName(newTermName(name)) || exposesName(newTypeName(name))
    )

    def exportedName(prefix: String, name: Name): String = {
      "import %s.%s".format(prefix, name)
    }
    // def exportedName(sym: Symbol): String = {
    //   if (sym.isStatic
    //   exportedName("_root_." + sym.owner.skipPackageObject.fullName, sym.name)
    // }

    def extraCodeToEvaluate(req: Request): String = ""
    def resultExtractionCode(req: Request): String = ""

    private def shortName = this.getClass.getName split "[.$]" last
    override def toString = "%s(%s)".format(shortName, member)  // + referencedNames.mkString(" (refs: ", ", ", ")")
  }

  class GenericHandler(member: Tree) extends MemberHandler(member)

  class ValHandler(member: ValDef) extends MemberDefHandler(member) {
    val maxStringElements = 1000  // no need to mkString billions of elements
    override def definesValue = true

    override def resultExtractionCode(req: Request): String = {
      val isInternal = isUserVarName(name) && req.lookupTypeOf(name) == "Unit"
      if (!mods.isPublic || isInternal) ""
      else {
        // if this is a lazy val we avoid evaluating it here
        val resultString =
          if (mods.isLazy) codegenln(false, "<lazy>")
          else any2stringOf(req fullPath name, maxStringElements)

        val vidString =
          if (replProps.vids) """" + " @ " + "%%8x".format(System.identityHashCode(%s)) + " """.trim.format(req fullPath name)
          else ""

        """ + "%s%s: %s = " + %s""".format(prettyName, vidString, string2code(req seenTypeOf name), resultString)
      }
    }
  }

  class DefHandler(member: DefDef) extends MemberDefHandler(member) {
    private def vparamss = member.vparamss
    private def isMacro = member.mods.hasFlag(scala.reflect.internal.Flags.MACRO)
    // true if not a macro and 0-arity
    override def definesValue = !isMacro && flattensToEmpty(vparamss)
    override def resultExtractionCode(req: Request) =
      if (mods.isPublic) codegenln(name, ": ", req.seenTypeOf(name)) else ""
  }

  class AssignHandler(member: Assign) extends MemberHandler(member) {
    val Assign(lhs, rhs) = member
    val name = newTermName(freshInternalVarName())

    override def definesTerm = Some(name)
    override def definesValue = true
    override def extraCodeToEvaluate(req: Request) =
      """val %s = %s""".format(name, lhs)

    /** Print out lhs instead of the generated varName */
    override def resultExtractionCode(req: Request) = {
      val lhsType = string2code(req lookupTypeOf name)
      val res     = string2code(req fullPath name)

      """ + "%s: %s = " + %s + "\n" """.format(lhs, lhsType, res) + "\n"
    }
  }

  class ModuleHandler(module: ModuleDef) extends MemberDefHandler(module) {
    override def definesTerm = Some(name)
    override def definesValue = true
    override def isLegalTopLevel = true

    override def resultExtractionCode(req: Request) = codegenln("defined module ", name)
  }

  class ClassHandler(member: ClassDef) extends MemberDefHandler(member) {
    override def definesType = Some(name.toTypeName)
    override def definesTerm = Some(name.toTermName) filter (_ => mods.isCase)
    override def isLegalTopLevel = true

    override def resultExtractionCode(req: Request) =
      codegenln("defined %s %s".format(keyword, name))
  }

  class TypeAliasHandler(member: TypeDef) extends MemberDefHandler(member) {
    private def isAlias = mods.isPublic && treeInfo.isAliasTypeDef(member)
    override def definesType = Some(name.toTypeName) filter (_ => isAlias)

    override def resultExtractionCode(req: Request) =
      codegenln("defined type alias ", name) + "\n"
  }

  class ImportHandler(imp: Import) extends MemberHandler(imp) {
    val Import(expr, selectors) = imp
    def exprSymbol = afterTyper(expr.symbol match {
      case null | NoSymbol  => NoSymbol
      case sym              => sym
    })

    def targetType: Type = intp.typeOfExpression("" + expr) match {
      case NoType   => rootMirror.getModuleIfDefined("" + expr + ".package").tpe
      case tpe      => tpe
    }
    def isExprPackage = exprSymbol.isPackage

    def fullExprPath = "" + expr
    //
    //   if (exprSymbol.isStatic) "_root_." + exprSymbol.fullName
    //   else "" + expr
    // ) tap println

    override def isLegalTopLevel = true

    override def exportedName(prefix: String, name: Name): String = printResult("exportedName" + ((prefix, name))) {
      super.exportedName("" + expr, name)
    }
    //
    // override def exportedName(prefix: String, name: Name): String = ({
    //   selectors foreach {
    //     case sel @ ImportSelector(old, _, `name`, _) =>
    //       if (old == name)
    //         return "import %s.%s".format(fullExprPath, name)
    //       else
    //         return "import %s.{ %s => %s }".format(fullExprPath, old, name)
    //     case _ =>
    //   }
    //   super.exportedName(prefix, name)
    // }) tap println

    //
    // def createImportForName(name: Name): String = {
    //   selectors foreach {
    //     case sel @ ImportSelector(old, _, `name`, _)  => return "import %s.{ %s }".format(fullExprPath, sel)
    //     case _ => ()
    //   }
    //   "import %s.%s".format(fullExprPath, name)
    // }
    // TODO: Need to track these specially to honor Predef masking attempts,
    // because they must be the leading imports in the code generated for each
    // line.  We can use the same machinery as Contexts now, anyway.
    def isPredefImport = isReferenceToPredef(expr)

    // wildcard imports, e.g. import foo._
    private def selectorWild    = selectors filter (_.name == nme.USCOREkw)
    // renamed imports, e.g. import foo.{ bar => baz }
    private def selectorRenames = selectors map (_.rename) filterNot (_ == null)

    /** Whether this import includes a wildcard import */
    val importsWildcard = selectorWild.nonEmpty

    override def importedSymbols = individualSymbols ++ wildcardSymbols

    lazy val individualSymbols: List[Symbol] =
      beforePickler(individualNames map (targetType nonPrivateMember _))

    lazy val wildcardSymbols: List[Symbol] =
      if (importsWildcard) beforePickler(importableMembers(targetType)) else Nil

    /** Complete list of names imported by a wildcard */
    lazy val wildcardNames: List[Name]   = afterTyper(wildcardSymbols map (_.name))
    lazy val individualNames: List[Name] = afterTyper(selectorRenames filterNot (_ == nme.USCOREkw) flatMap (_.bothNames))

    /** The names imported by this statement */
    override def importedNames: List[Name] = wildcardNames ++ individualNames

    def importString = imp.toString
    override def resultExtractionCode(req: Request) = codegenln(importString) + "\n"
  }
}
