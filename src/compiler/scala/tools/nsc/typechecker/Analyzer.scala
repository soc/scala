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
    object NoSkolems extends TypeMap {
      override def apply(tp: Type): Type = tp match {
        case TypeRef(pre, sym, args) if sym.isTypeSkolem               => mapOver(copyTypeRef(tp, pre, sym.deSkolemize, args))
        case PolyType(tparams, res) if tparams exists (_.isTypeSkolem) => mapOver(polyType(tparams map (_.deSkolemize), res))
        case _                                                         => mapOver(tp)
      }
    }
    def removeSymbolSkolems(sym: Symbol) {
      if (sym eq null) return
      sym.info.paramss.flatten foreach (_ modifyInfo NoSkolems)
      sym.info.typeParams foreach (_ modifyInfo NoSkolems)
      sym modifyInfo NoSkolems
    }

    // object TreeNoSkolems extends Transformer {
    //   override def apply(tree: Tree): Tree = tree match {
    //     case TypeRef(pre, sym, args) if sym.isTypeSkolem               => mapOver(copyTypeRef(tp, pre, sym.deSkolemize, args))
    //     case PolyType(tparams, res) if tparams exists (_.isTypeSkolem) => mapOver(polyType(tparams map (_.deSkolemize), res))
    //     case _                                                         => mapOver(tp)
    //   }
    // }

    def removeAllSkolems(tree: Tree): Tree = {
      for (t <- tree) {
        removeSymbolSkolems(t.symbol)

        val old = t.tpe
        t modifyType NoSkolems
        if (t.tpe ne old)
          println(s"$old => ${t.tpe}")

        // if (t.symbol != null) {
        //   if (t.symbol.isTypeSkolem) {
        //     val old = t.symbol
        //     try t setSymbol t.symbol.deSkolemize
        //     catch { case _: UnsupportedOperationException => }
        //     println(s"$old => ${t.symbol}")
        //   }
        //   val oldInfo = t.symbol.info
        //   t.symbol modifyInfo NoSkolems
        //   if (oldInfo ne t.symbol.info)
        //     println(s"$oldInfo => ${t.symbol.info}")
        // }
        // val old = t.tpe
        // t modifyType NoSkolems
        // if (t.tpe ne old)
        //   println(s"$old => ${t.tpe}")
      }
      tree
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
        for (unit <- currentRun.units)
          removeAllSkolems(unit.body)
      }
      def apply(unit: CompilationUnit) {
        try {
          val typer = newTyper(rootContext(unit))
          unit.body = typer.typed(unit.body)
          if (global.settings.Yrangepos.value && !global.reporter.hasErrors) global.validatePositions(unit.body)
          for (workItem <- unit.toCheck) workItem()
          if (settings.lint.value)
            typer checkUnused unit

          unit.body = removeAllSkolems(unit.body)
        }
        finally {
          unit.toCheck.clear()
        }
      }
    }
  }
}
