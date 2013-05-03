/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import scala.reflect.internal.util.Statistics

/** The main attribution phase.
 */
trait Analyzer extends AnyRef
            with Contexts
            with Namers
            with Typers
            with Infer
            with Implicits
            with EtaExpansion
            with SyntheticMethods
            with Unapplies
            with Macros
            with NamesDefaults
            with TypeDiagnostics
            with ContextErrors
            with StdAttachments
            with AnalyzerPlugins
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

    class Owners(val unit: CompilationUnit, val owners: List[Tree]) {
      override def toString = (
        "" + unit + ":" + owners.head.pos.show + " " + owners.head //.pos
        // "" + unit + " with owners:\n  " + (owners take 2).mkString("\n  -> ") + " ... (" + (owners drop 2).size + " more)"
      )
      override def equals(other: Any) = other match {
        case that: Owners => unit == that.unit && owners == that.owners
        case _            => super.equals(other)
      }
      override def hashCode = unit.## + owners.##
    }

    import scala.reflect.internal.TypesStats.typerNanos
    val global: Analyzer.this.global.type = Analyzer.this.global
    val phaseName = "typer"
    val runsAfter = List[String]()
    val runsRightAfter = Some("packageobjects")
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
        for (unit <- currentRun.units) {
          applyPhase(unit)
          undoLog.clear()
        }
        if (Statistics.canEnable) Statistics.stopTimer(typerNanos, start)
        val trees = perRunCaches.newMap[Int, List[Tree]]() withDefaultValue Nil
        val ownersMap = perRunCaches.newMap[Tree, List[Owners]]() withDefaultValue Nil

        currentRun.units foreach { unit =>
          unit.body foreach { t =>
            if (t != EmptyTree)
              trees(t.##) ::= t
          }
        }
        object stacker extends Traverser {
          var stack: List[Tree] = Nil
          var currentUnit: CompilationUnit = _

          override def traverse(t: Tree) {
            val o = new Owners(currentUnit, stack)
            ownersMap(t) = (o :: ownersMap(t)).distinct
            stack ::= t
            try super.traverse(t)
            finally stack = stack.tail
          }
          def apply(unit: CompilationUnit) {
            currentUnit = unit
            traverse(unit.body)
          }
        }
        currentRun.units foreach (u => stacker(u))

        val dups = trees.toList filter (_._2.tail.nonEmpty)
        println(s"${trees.size} trees, ${dups.size} shared trees")
        dups foreach println

        val dups2 = ownersMap.toList filter (_._2.tail.nonEmpty)
        dups2 foreach { case (t, o1 :: o2 :: _) =>
          if (t != EmptyTree) {
            val u = o1.unit
            u.warning(t.pos, "Shared tree")
            u.warning(o1.owners.head.pos, "  owner #1")
            u.warning(o2.owners.head.pos, "  owner #2")

            // println(s"$t/sym=${t.symbol}\n")
            // println("in " + u + ":" + t.pos.line)
            // println(s"+0   ${t.##}   $t")
            var os1 = o1.owners take (o1.owners.indexWhere(_.isInstanceOf[ValOrDefDef]) + 1)
            var os2 = o2.owners take (o2.owners.indexWhere(_.isInstanceOf[ValOrDefDef]) + 1)
            var pairs: List[(Tree, Tree)] = Nil

            while (os1.nonEmpty || os2.nonEmpty) {
              val x1 = if (os1.isEmpty) EmptyTree else try os1.head finally os1 = os1.tail
              val x2 = if (os2.isEmpty) EmptyTree else try os2.head finally os2 = os2.tail
              pairs ::= ((x1, x2))
            }
            for (((x1, x2), n0) <- pairs.reverse.zipWithIndex) {
              val n = n0 + 1
              println(s"+$n  ${x1.##}  $x1")
              println(s"+$n  ${x2.##}  $x2")
              println("")
            }
            println("")
          }
        }
      }
      def apply(unit: CompilationUnit) {
        try {
          val typer = newTyper(rootContext(unit))
          unit.body = typer.typed(unit.body)
          if (global.settings.Yrangepos && !global.reporter.hasErrors) global.validatePositions(unit.body)
          for (workItem <- unit.toCheck) workItem()
          if (settings.lint)
            typer checkUnused unit
        }
        finally {
          unit.toCheck.clear()
        }
      }
    }
  }
}
