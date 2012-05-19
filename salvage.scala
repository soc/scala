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
