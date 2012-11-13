/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package typechecker

import scala.collection.{ mutable, immutable }
import symtab.Flags

trait StaticAnalysis {
  val global: Global

  import global._
  import definitions.{ isPhantomClass }

  // class TWrap(val tree: Tree) {
  //   override def toString = "" + tree
  //   override def hashCode = tree.##
  //   override def equals(x: Any) = x match {
  //     case that: TWrap => tree eq that.tree
  //     case _           => false
  //   }
  // }

  def staticAnalysis(units: List[CompilationUnit]) = new WorldStats(units)

  class WorldStats(val units: List[CompilationUnit]) {
    world =>

    def log(msg: String) = Console.err.println(msg)

    private def dealias(s: Symbol): Symbol = s match {
      case s: AliasTypeSymbol => dealias(s.info.normalize.typeSymbol)
      case _                  => s
    }

    log(s"Reading top level infos for ${units.size} units...")
    val top: List[TopDefInfo]      = readInfos()
    val defs: Map[Symbol, DefInfo] = top flatMap (info => info.defns map (mdef => mdef.symbol ->
    val refs: Map[Symbol, RefInfo] = top flatMap (info => info.defns map (mdef => mdef.symbol ->

    log(s"Calculating outrefs...")
    locally {
      top foreach (_.outRefs)
    }
    log(s"Done calculating outrefs.")

    def topInUnit(unit: CompilationUnit): List[TopDefInfo] = top filter (_.unit == unit)
    def topFor(s: Symbol): Option[TopDefInfo]              = top find (_.msym == s)
    def topFor(t: MemberDef): Option[TopDefInfo]           = top find (_.mdef == t)
    def enclosingTopFor(s: Symbol): Option[TopDefInfo]     =
      if (s eq NoSymbol) None else topFor(s) orElse enclosingTopFor(s.owner)

    // val symSource = currentRun.symSource // AbstractFile
    // val symData = currentRun.symData     // PickleBuffer
    // val topSyms1: List[Symbol] = currentRun.symSource.keys.toList.sortWith(_ isLess _)
    // val topSyms2: List[Symbol] = top map (_.msym) sortWith (_ isLess _)
    // if (topSyms1 != topSyms2) {
    //   println("In topSyms1 but not 2: " + topSyms1.filterNot(topSyms2 contains _))
    //   println("In topSyms2 but not 1: " + topSyms2.filterNot(topSyms1 contains _))
    // }

    def defines(sym: Symbol): List[TopLevelInfo]           = top filter (_ defines sym)
    def references(sym: Symbol): List[TopLevelInfo]        = top filter (_ references sym)
    def references(info: TopLevelInfo): List[TopLevelInfo] = top filter (_.defSyms exists info.references)

    implicit lazy val topLevelInfoOrdering = Ordering[String] on ((x: TopLevelInfo) => x.path + "/" + x.fullName)

    abstract class RefInfo {
      def ref: RefTree
      def defn: DefInfo
      def symbol: Symbol = defn.symbol
    }
    abstract class DefInfo {
      def mdef: Tree
      def symbol: Symbol
    }
    class Def(val mdef: MemberDef) extends DefInfo {
      def symbol = defn.symbol
    }
    class ExtDef(val symbol: Symbol) extends DefInfo {
      def mdef = EmptyTree
    }
    class Ref(val ref: RefTree, val defn: DefInfo) extends RefInfo {
      def this(sym: Symbol) = this(ref, new ExtDef(sym))
    }

    class TopDefInfo(
      val mdef: MemberDef,
      val unit: CompilationUnit
    ) {
      val msym      = mdef.symbol
      val trees     = mdef collect { case t: Tree => t }
      val symbols   = trees collect { case t if t.symbol != null && t.symbol != NoSymbol => t.symbol.initialize }
      val defns     = trees collect { case md: MemberDef if md.symbol != null => md }
      val refs      = trees collect { case rt: RefTree if rt.symbol != null => rt }
      val defSyms   = defns map (_.symbol) filterNot (_ hasFlag Flags.PACKAGE)
      val refSyms   = refs map (_.symbol) filterNot (_ hasFlag Flags.PACKAGE)

      def codefinedTopLevel  = top filter (_.unit == unit)
      def codefinedDefSyms   = codefinedTopLevel flatMap (_.defSyms)
      def codefinedOutSyms   = codefinedTopLevel flatMap (_.outSyms)
      def codefinedDefAndOut = codefinedDefSyms ++ codefinedOutSyms

      def path                    = unit.source.path
      def defines(sym: Symbol)    = defSyms contains sym
      def references(sym: Symbol) = refSyms contains sym
      def fullName                = msym.fullNameString
      def simpleName              = msym.decodedName
      def pkg                     = msym.enclosingPackage
      def extClasses              = clean(extSyms map (_.enclClass))

      lazy val outRefs = refs flatMap (rt =>
        topFor(rt.symbol) filterNot (_.unit == unit) match {

        }

        definitionFor(rt.symbol) match {
          case Some(info)
        }


      lazy val outRefs   = refSyms flatMap (world defines _) filterNot (_ eq this)
      lazy val inRefs    = top filter (x => (x.outRefs contains this) && (x != this))
      lazy val refSymMap: Map[Symbol, List[Symbol]] =
        refSyms groupBy (world defines _ headOption) collect { case (Some(t), syms) => t.msym -> syms }

      private def excludeFromExt(s: Symbol) = (
           (codefinedDefAndOut contains s)
        || (codefinedDefAndOut contains dealias(s))
        || (isPhantomClass(s))
        || (s.isJavaDefined)
      )
      private def isExt(s: Symbol) = !excludeFromExt(s)

      def inSyms   = inRefs map (_.msym)
      def outSyms  = outRefs map (_.msym)
      def extSyms  = refSyms filter isExt
      def termSyms = symbols collect { case s: TermSymbol => s }
      def typeSyms = symbols collect { case s: TypeSymbol => s }

      private def clean(xs: List[Symbol]): List[Symbol] =
        xs filterNot (s => (s eq NoSymbol) || (s.owner == NoSymbol) || (s hasFlag Flags.PACKAGE)) distinct

      private def groupSyms(syms: List[Symbol]): List[String] = {
        val uniq   = syms.distinct
        val counts = syms groupBy (x => x) mapValues (_.size)
        def count(s: Symbol) = counts.getOrElse(s, 1) match {
          case 1 => ""
          case n => "/" + n
        }
        def str(s: Symbol) = s.decodedName + count(s)
        val lines = uniq.groupBy(_.enclosingPackage).toList sortBy (_._1.fullName) map {
          case (p, sym :: Nil) => p.fullName + "." + sym.decodedName
          case (p, syms1)      => syms1.map(str).sorted.distinct.mkString(p.fullName + ".{ ", ", ", " }")
        }
        lines.toList
      }
      private def in_s  = if (inSyms.isEmpty) "" else groupSyms(inSyms).mkString("\n  Incoming {\n    ", "\n    ", "\n  }")
      private def out_s = if (outSyms.isEmpty) "" else groupSyms(outSyms).mkString("\n  Outgoing {\n    ", "\n    ", "\n  }")
      private def ext_s = if (extClasses.isEmpty) "" else groupSyms(extClasses).mkString("\n  External {\n    ", "\n    ", "\n  }")

      override def toString = f"""
        |$path
        |${msym.fullNameString} {
        |  Symbol:${symbols.size}  (TermSymbol: ${termSyms.size}, TypeSymbol: ${typeSyms.size})
        |    Tree:${trees.size}  (MemberDef: ${defns.size}, RefTree: ${refs.size})
        |$in_s$out_s$ext_s
        |}
        |""".trim.stripMargin
    }

    private def readInfos() = (units flatMap readTopLevelInfos).sorted
    private def readTopLevelInfos(unit: CompilationUnit): List[TopLevelInfo] = {
      def loop(t: Tree): List[TopLevelInfo] = t match {
        case PackageDef(pid, stats) =>
          stats flatMap loop
        case md @ ModuleDef(_, _, Template(_, _, stats)) =>
          val mod = new TopLevelInfo(md, unit)
          mod :: (stats flatMap loop)
        case md: MemberDef =>
          new TopLevelInfo(md, unit) :: Nil
        case _ =>
          Nil
      }
      loop(unit.body)
    }
  }
}
