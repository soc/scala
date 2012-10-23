def replScope: Scope              = if (replContext eq null) EmptyScope else replContext.scope
def replImports: List[ImportInfo] = if (replContext eq null) Nil else replContext.imports
def replOwner: Symbol             = if (replContext eq null) NoSymbol else replContext.owner

def contextLookup(name: Name): Symbol = {
  var res: Symbol = NoSymbol
  var ctx = replContext
  while (res == NoSymbol && ctx.outer != ctx) {
    val s = ctx.scope lookup name
    if (s != NoSymbol)
      res = s
    else
      ctx = ctx.outer
  }
  res
}

protected def newCompiler(settings: Settings, reporter: Reporter): ReplGlobal = {
  new ReplGlobal(settings, reporter)
def newReplContext() = analyzer.rootContext(new CompilationUnit(new BatchSourceFile("<root>", "")))
def updateReplContext(line: String, trees: List[Tree]) = {
  val cunit = new CompilationUnit(new BatchSourceFile("<req>", line))
  val templ = Template(Nil, emptyValDef, trees)
  
  echo("replContext.make" + ((cunit, templ, replOwner, replScope, replImports)))
  replContext = replContext.make(cunit, templ, replOwner, replScope, replImports)
}

def savingTyperContext[T](body: => T): T = {
  val saved = typer.context
  typer.context = newReplContext()
  try body
  finally typer.context = saved
}

def calculateTreeType(t: Tree): ((Scope, Type)) = {
  val saved = t.tpe
  t foreach {
    case Ident(n) => symbolOfTerm("" + n) match {
      case NoSymbol =>
      case s        => echo("Entering " + s) ; typer.context.scope enter s
    }
    case _ =>
  }
  val tpe = (typer typed t).tpe
  try ((typer.context.scope, tpe))
  finally t.tpe = saved
}
  

def dumpTrees(trees: List[Tree]) {
  savingTyperContext {
    trees foreach { t =>
      val (ctx, tpe) = calculateTreeType(t)
      echo("  " + ((t, ctx, tpe)))
    }
  }
}


    // All package symbols found underneath the given package symbol.
    def allPackages(sym: Symbol): List[Symbol] = {
      val packages = sym.moduleClass.info.nonPrivateMembers.filter(_.isPackage)
      packages ++ (packages flatMap allPackages)
    }
    // All classes found at the given level or below.
    def allClasses(sym: Symbol): List[Symbol] = {
      allPackages(sym) flatMap (_.info.nonPrivateMembers filter (_.isClass))
    }
    def allStandaloneClasses(sym: Symbol) = {
      allClasses(sym) filter { s =>
        !s.name.toString.contains("$") && {
          try s.initialize
          catch { case _ => System.err.println("Squashing exception initializing " + s.name) }

          (
               s.owner.isPackageClass
            && !s.isDeferred
            && !s.isAbstractClass
            && !s.isTrait
            && s.isPublic
            && s.primaryConstructor.isPublic
            && (s.primaryConstructor != NoSymbol)
            && (s.primaryConstructor.paramss.flatten forall (s => { s.initialize ; true }))
          )
        }
      }
    }
    def usableType(tp: Type): Type = {
      tp map { t =>
        if (t.typeSymbol.isTypeParameterOrSkolem) usableType(t.typeSymbol.info.bounds.hi)
        else t
      }
    }
    def usableParam(sym: Symbol) = {
      val tsym = sym.tpe.typeSymbol

      if (tsym.isTypeParameterOrSkolem) "java.lang.String"
      else if (tsym.isAbstractType) usableType(sym.info.bounds.hi)
      else usableType(sym.tpe)
    }

    def constructions(name: String): List[String] = constructions(name, x => "zero[" + usableParam(x) + "]")
    def constructions(name: String, f: Symbol => String): List[String] = {
      (
        allStandaloneClasses(getRequiredModule("scala.collection"))
          map (_.primaryConstructor)
          map (x => "new " + x.info.finalResultType.typeConstructor +
            x.paramss.map(ps => ps map (p => f(p)) mkString ("(", ", ", ")")).mkString("")
          )
      )
    }
    def demo() = constructions("scala.collection") foreach println

++ b/src/compiler/scala/tools/nsc/interpreter/IMain.scala
  implicit lazy val powerNameOrdering: Ordering[Name]     = Ordering[String] on (_.toString)
  implicit lazy val powerSymbolOrdering: Ordering[Symbol] = Ordering[Name] on (_.name)
  implicit lazy val powerTypeOrdering: Ordering[Type]     = Ordering[Symbol] on (_.typeSymbol)

  def allDefinedNames = stickyNames.keys.toList ++ definedNameMap.keys.toList sorted
  def pathToName(name: Name): String =
    requestForName(name) map (_ fullPath name) getOrElse ("" + name)
  def handleRedefinition(name: Name, old: Request, req: Request) = {
    name match {
      case name: TypeName =>
      case name: TermName =>
  }
    req.referencedNames foreach { name =>
      stickyNames get name match {
        case Some(n)  => replwarn("Sticky name `" + name + "` rewritten.")
        case _        => referencedNameMap(name) = req
      }
    }
      val exists = definedNameMap contains name
      if (exists && !isStickyMode)
        echo("Can't displace sticky value `" + name + "`.")
      else {
        if (exists)
          handleRedefinition(name, definedNameMap(name), req)
        if (isStickyMode)
          updateSticky(name, req)

    if (isStickyMode) {
      req.importedNames foreach { name =>
        updateSticky(name, req)
      }
    }
  }
    // if (result == IR.Success)
    //   directlyBoundNames += newTermName(name)

    val definedNames  = handlers flatMap (_.definedNames) //filterNot isStickyName
    val importedNames = handlers flatMap (_.importedNames) //filterNot isStickyName
    def definedOrImportedNames = definedNames ++ importedNames distinct
    def definedOrImportedSymbols = (
         definedSymbols.values.toList
      ++ handlers.flatMap(_.importedSymbols)
    )
      importsCode(referencedNames.toSet filterNot isStickyName)
    (stickyNames get name) orElse (definedNameMap get name)
    prevRequests.reverseIterator filter (_.definedNames contains name) toList
  def stickySymbols  = stickyNames.toList flatMap { case (name, req) => req.definedOrImportedSymbols find (_.name == name) }
  def definedSymbols = prevRequests.reverseIterator flatMap (_.definedSymbols.values)
  def namedDefinedTerms = definedTerms filterNot (x => isUserVarName("" + x) || (stickyNames contains x))
  private def findName(name: Name) = (stickySymbols ++ definedSymbols) find (_.name == name)
  private var isStickyMode = false
  def sticky = stickyNames.keys.toList.sorted filterNot (_ startsWith "$")
  def stickily[T](body: => T): T = {
    val saved = isStickyMode
    isStickyMode = true
    try body
    finally isStickyMode = saved
  }
  private def updateSticky(name: Name, req: Request) {
    echo("Sticky: " + name)
    stickyNames(name) = req
  }
  private lazy val stickyNames       = mutable.Map[Name, Request]()
  def isStickyName(name: Name) = stickyNames contains name

++ b/src/compiler/scala/tools/nsc/interpreter/Imports.scala
    val session = importHandlers filterNot (_.targetType eq NoType) map { mh =>
++ b/src/compiler/scala/tools/nsc/interpreter/MemberHandlers.scala
  import intp.{ Request, global, naming, isStickyName }
  import definitions.{ AnyClass, ObjectClass }
  // Members of Any or AnyRef cannot be imported because the inherited version
  // always takes precedence.
  lazy val unimportableNames: Set[Name] = (
    AnyClass.info.nonPrivateMembers.map(_.name).toSet ++
    ObjectClass.info.nonPrivateMembers.map(_.name)
  )
  def importableMembers(tp: Type) = tp.nonPrivateMembers filterNot (sym => unimportableNames(sym.name))

    def definedOrImportedNames = definedNames ++ importedNames distinct

    def importedSymbols: List[Symbol] = Nil
    override def importedSymbols = individualSymbols ++ wildcardSymbols
    def isImportable(sym: Symbol) = (ObjectClass.tpe nonPrivateMember sym.name) == NoSymbol
      beforePickler(individualNames map (targetType nonPrivateMember _) filter isImportable)
    lazy val wildcardNames: List[Name]   = (
      wildcardSymbols map (_.name) filterNot isStickyName
    )
    lazy val individualNames: List[Name] = (
      selectorRenames filterNot (_ == nme.USCOREkw) flatMap (_.bothNames) filterNot isStickyName
    )
++ b/src/compiler/scala/tools/nsc/interpreter/Power.scala
    intp stickily {
    }
++ b/src/compiler/scala/tools/nsc/interpreter/ReplVals.scala
  final lazy val sticky                   = intp.sticky
++ b/test/files/jvm/serialization.scala
