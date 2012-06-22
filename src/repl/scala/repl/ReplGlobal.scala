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

  private type AnyContext = scala.tools.nsc.typechecker.Contexts#Context
  private val rlogging = sys.props contains "rlog"
  def rlog(label: String, args: Any*) {
    def str(x: Any): String = x match {
      case null                    => "null"
      case Literal(const)          => const.escapedStringValue
      case Import(expr, selectors) => "Import(%s, %s)".format(str(expr.symbol), selectors.map(str).mkString(", "))
      case t: Tree                 => t.summaryString
      case c: AnyContext           => "Context(%s%s)".format(str(c.owner), if (c.tree.symbol eq c.owner) "" else ", " + str(c.tree.symbol))
      case sym: Symbol             => sym.defString
      case x                       => "" + x
    }
    if (rlogging)
      Console.println(args.map(str).mkString(label + "(", ", ", ")"))
  }
  private var suppressRlog = false
  private def suppressingRlog[T](body: => T): T = {
    suppressRlog = true
    try body
    finally suppressRlog = false
  }

  override def signalDone(context: analyzer.Context, old: Tree, result: Tree) {
    if (old eq result)
      rlog("signalDone", context, "_", result, result.tpe)
    else
      rlog("signalDone", context, old, result, result.tpe)
  }
  override def signalParseProgress(pos: Position) {
    rlog("signalParseProgress", pos)
  }
  override def registerContext(c: analyzer.Context) {
    super.registerContext(c)
    if (!suppressRlog)
      suppressingRlog(rlog("registerContext", c))
  }
  override def registerTopLevelSym(sym: Symbol) {
    rlog("registerTopLevelSym", sym)
  }

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
