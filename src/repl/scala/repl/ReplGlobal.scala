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
  
  lazy val initialReplScope = newReplScope()

  def newReplScope(): Scope = {
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
    sc.scope
  }

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
