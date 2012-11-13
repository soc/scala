/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import scala.reflect.internal.util.Statistics
import scala.reflect.io.NoAbstractFile

/** The main attribution phase.
 */
trait Analyzer extends AnyRef
            with Contexts
            with Namers
            with Typers
            with Infer
            with Implicits
            with Variances
            with EtaExpansion
            with SyntheticMethods
            with Unapplies
            with Macros
            with NamesDefaults
            with TypeDiagnostics
            with ContextErrors
            with StdAttachments
{
  val global : Global
  import global._

  object namerFactory extends SubComponent {
    val global: Analyzer.this.global.type = Analyzer.this.global
    val phaseName = "namer"
    val runsAfter = List[String]("parser")
    val runsRightAfter = None
    def newPhase(_prev: Phase): StdPhase = new StdPhase(_prev) {
      override val checkable = false
      override def keepsTypeParams = false

      def apply(unit: CompilationUnit) {
        newNamer(rootContext(unit)).enterSym(unit.body)
      }
    }
  }

  object packageObjects extends SubComponent {
    val global: Analyzer.this.global.type = Analyzer.this.global
    val phaseName = "packageobjects"
    val runsAfter = List[String]()
    val runsRightAfter= Some("namer")

    def newPhase(_prev: Phase): StdPhase = new StdPhase(_prev) {
      override val checkable = false
      import global._

      val openPackageObjectsTraverser = new Traverser {
        override def traverse(tree: Tree): Unit = tree match {
          case ModuleDef(_, _, _) =>
            if (tree.symbol.name == nme.PACKAGEkw) {
              openPackageModule(tree.symbol, tree.symbol.owner)
            }
          case ClassDef(_, _, _, _) => () // make it fast
          case _ => super.traverse(tree)
        }
      }

      def apply(unit: CompilationUnit) {
        openPackageObjectsTraverser(unit.body)
      }
    }
  }

  object typerFactory extends SubComponent {
    import scala.reflect.internal.TypesStats.typerNanos
    val global: Analyzer.this.global.type = Analyzer.this.global
    val phaseName = "typer"
    val runsAfter = List[String]()
    val runsRightAfter = Some("packageobjects")

    def isIndependent(unit: CompilationUnit): Boolean = {
      def tops(t: Tree): List[MemberDef] = t match {
        case PackageDef(_, stats) => stats flatMap tops
        case md: MemberDef        => md :: Nil
        case _                    => Nil
      }
      def where(s: Symbol) = currentRun symbolSourceFile s match {
        case NoAbstractFile => "" + s
        case f              => s"$s in $f"
      }
      val toplevel = tops(unit.body) map (_.symbol)
      // println("toplevel: " + toplevel)
      val bases = toplevel flatMap (_.ancestors)
      val bases1 = bases filter (currentRun compiles _)
      // println("bases1 = " + bases1)
      val bases2 = bases1 filterNot (currentRun.symSource get _ contains unit.source.file)
      // println("bases2 = " + bases2)
      toplevel foreach { sym =>
        val deps = sym.ancestors filter (b => (currentRun compiles b) && !(currentRun.symSource get b contains unit.source.file))
        if (deps.isEmpty)
          println(s"$sym is independent.")
        else {
          // val deps_s = deps map (s => currentRun.symSource.get(s).fold("" + s)("" + s + " in " + _)) mkString "\n  "
          val deps_s = deps map where mkString "\n  "
          println(s"$sym depends on:\n  $deps_s")
        }
        // deps
      }
      bases2.isEmpty
    }

    def newPhase(_prev: Phase): StdPhase = new StdPhase(_prev) {
      override def keepsTypeParams = false
      resetTyper()
      // the log accumulates entries over time, even though it should not (Adriaan, Martin said so).
      // Lacking a better fix, we clear it here (before the phase is created, meaning for each
      // compiler run). This is good enough for the resident compiler, which was the most affected.
      undoLog.clear()
      override def run() {
        val start = if (Statistics.canEnable) Statistics.startTimer(typerNanos) else null
        global.echoPhaseSummary(this)
        val units = currentRun.units.toList
        // units foreach (unit => unit.body foreach (t => println(t.symbol)))
        val (independent, dependent) = units partition isIndependent
        // println("dependent: " + dependent)
        // println("independent: " + independent)
        for (unit <- units) {
          applyPhase(unit)
          undoLog.clear()
        }
        if (Statistics.canEnable) Statistics.stopTimer(typerNanos, start)
      }
      def apply(unit: CompilationUnit) {
        try {
          val typer = newTyper(rootContext(unit))
          unit.body = typer.typed(unit.body)
          if (global.settings.Yrangepos.value && !global.reporter.hasErrors) global.validatePositions(unit.body)
          for (workItem <- unit.toCheck) workItem()
          if (settings.lint.value)
            typer checkUnused unit
        }
        finally {
          unit.toCheck.clear()
        }
      }
    }
  }
}
