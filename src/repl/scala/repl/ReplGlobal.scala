/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.repl

import scala.tools.nsc._
import typechecker.Analyzer

/** A layer on top of Global so I can guarantee some extra
 *  functionality for the repl.  It doesn't do much yet.
 */
trait ReplGlobal extends Global {
  self =>

  // Marker tree for incomplete input
  case object IncompleteTree extends Tree { }

  override lazy val analyzer = new {
    val global: ReplGlobal.this.type = ReplGlobal.this
  } with Analyzer {
    override def newTyper(context: Context): Typer = new Typer(context) {
      override def typed(tree: Tree, mode: Int, pt: Type): Tree = {
        super.typed(tree, mode, pt) tap {
          case res @ Ident(name) if !tree.symbol.hasPackageFlag && !name.toString.startsWith("$") =>
            repldbg("typed %s: %s".format(name, res.tpe))
          case _ =>
        }
      }
    }
  }
  import analyzer.{ Context, NoContext, rootContext }
  //
  // var replContext: Context = _
  //
  // def initReplContext() {
  //   replContext = newReplContext
  // }
  def newReplContext() = {
    var sc = (
      NoContext.make(
        Template(Nil, emptyValDef, Nil) setSymbol NoSymbol setType NoType,
        rootMirror.RootClass,
        rootMirror.RootClass.info.decls
      )
    )
    for (sym <- definitions.UnqualifiedPackages) {
      sc = sc.makeNewImport(sym)
      sc.depth += 1
    }
    val c = sc.make(newCompilationUnit(""), EmptyTree, sc.owner, sc.scope, sc.imports)
    // c.setReportErrors()
    c.implicitsEnabled = true
    c.enrichmentEnabled = true
    c
  }
  // def updateReplContext(sym: Symbol) {
  //   Console.println("updateReplContext(" + sym + ")")
  //   replContext = replContext.makeNewImport(sym) tap (x => Console println ((x, x.scope.toList)))
  // }
  // def updateReplContext(imp: Import) {
  //   Console.println("updateReplContext(" + imp + ")")
  //   replContext = replContext.makeNewImport(imp) tap (x => Console println ((x, x.scope.toList)))
  // }
  // def updateReplScope(syms: Iterable[Symbol]) {
  //   syms foreach (sym => replScope.lookupAll(sym.name) foreach (replScope unlink _))
  //   syms foreach (replScope enter _)
  //   replScope
  // }
  //
  //
  // def makeNewImport(sym: Symbol): Context =
  //   makeNewImport(gen.mkWildcardImport(sym))
  //
  // def makeNewImport(imp: Import): Context =
  //   make(unit, imp, owner, scope, new ImportInfo(imp, depth) :: imports)
  //
  // def make(tree: Tree, owner: Symbol, scope: Scope): Context =
  //   if (tree == this.tree && owner == this.owner && scope == this.scope) this
  //   else make0(tree, owner, scope)
  //
  // //
  // def newRootScope: Scope = {
  //   val s = newScope
  //
  //   mkWildcardImport
  //
  // lazy val replScope: Scope = {
  //   val s = newScope
  //
  //
  //   syms: Iterable[Symbol]): Scope = {
  //   syms foreach (sym => replScope.lookupAll(sym.name) foreach (replScope unlink _))
  //   syms foreach (replScope enter _)
  //   replScope
  // }
  // def replScope = _replScope

  object replPhase extends SubComponent {
    val global: ReplGlobal.this.type = ReplGlobal.this
    val phaseName = "repl"
    val runsAfter = List[String]("typer")
    val runsRightAfter = None
    def newPhase(_prev: Phase): StdPhase = new StdPhase(_prev) {
      def apply(unit: CompilationUnit) {
        repldbg("Running replPhase on " + unit.body)
        // newNamer(rootContext(unit)).enterSym(unit.body)
      }
    }
  }

  override protected def computePhaseDescriptors: List[SubComponent] = {
    addToPhasesSet(replPhase, "repl")
    super.computePhaseDescriptors
  }
}
