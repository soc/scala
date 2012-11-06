/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package typechecker

import util.TreeTracer
import scala.collection.{ mutable, immutable }

trait AnalyzerAPI {
  self: Analyzer =>

  import global._

  def newTyper(context: Context): Typer
  def newNamer(context: Context): Namer
  def finishTyper(): Unit

  // Subset of Typer explicitly exposed
  trait TypedTrees {
    def typed(tree: Tree, mode: Int, pt: Type): Tree
    def typedApply(tree: Apply, mode: Int, pt: Type): Tree
    def typedClassDef(cdef: ClassDef): ClassDef
    def typedModuleDef(mdef: ModuleDef): ModuleDef
    def typedDefDef(ddef: DefDef): DefDef
    def typedTypeDef(tdef: TypeDef): TypeDef
    def typedValDef(vdef: ValDef): ValDef
    def typedTemplate(templ: Template, parents1: List[Tree]): Template
    def typedStat(stat: Tree, exprOwner: Symbol): Tree

    /** Other typer methods

    def typed(tree: Tree, mode: Int, pt: Type): Tree
    def typedApply(fun: Tree, args: List[Tree]): Tree
    def typedArgs(args: List[Tree], mode: Int): List[Tree]
    def typedBlock(block: Block, mode: Int, pt: Type): Block
    def typedStats(stats: List[Tree], exprOwner: Symbol): List[Tree]

    def typedAnnotated(ann: Tree, arg1: Tree): Tree
    def typedAnnotation(ann: Tree, mode: Int, selfsym: Symbol, annClass: Symbol, requireJava: Boolean): AnnotationInfo
    def typedAppliedTypeTree(tpt: Tree, args: List[Tree]): Tree
    def typedArg(arg: Tree, mode: Int, newmode: Int, pt: Type): Tree
    def typedArrayValue(elemtpt: Tree, elems: List[Tree]): Tree
    def typedAssign(lhs: Tree, rhs: Tree): Tree
    def typedBind(name: Name, body: Tree): Tree
    def typedBlock(block: Block, mode: Int, pt: Type): Block
    def typedCase(cdef: CaseDef, pattpe: Type, pt: Type): CaseDef
    def typedCases(tree: Tree, cases: List[CaseDef], pattp: Type, pt: Type): List[CaseDef]
    def typedCompoundTypeTree(templ: Template): Tree
    def typedEta(expr1: Tree): Tree
    def typedFunction(fun: Function, mode: Int, pt: Type): Tree
    def typedHigherKindedType(tree: Tree, mode: Int, pt: Type): Tree
    def typedIdent(name: Name): Tree
    def typedIf(cond: Tree, thenp: Tree, elsep: Tree): Tree
    def typedImport(imp: Import): Import
    def typedLabelDef(ldef: LabelDef): LabelDef
    def typedMatch(tree: Tree, selector: Tree, cases: List[CaseDef]): Tree
    def typedNew(tpt: Tree): Tree
    def typedOperator(tree: Tree): Tree
    def typedPattern(tree: Tree, pt: Type): Tree
    def typedPos(pos: Position)(tree: Tree): Tree
    def typedQualifier(tree: Tree, mode: Int, pt: Type): Tree
    def typedRefinement(stats: List[Tree]): Unit  // !!!    don't return Unit!
    def typedReturn(expr: Tree): Tree
    def typedSelect(qual: Tree, name: Name): Tree
    def typedStat(stat: Tree): Tree
    def typedSuper(qual: Tree, mix: TypeName): Tree
    def typedTemplate(templ: Template, parents1: List[Tree]): Template
    def typedThis(qual: Name): Tree
    def typedType(tree: Tree): Tree
    def typedType(tree: Tree, mode: Int): Tree
    def typedTypeConstructor(tree: Tree, mode: Int): Tree
    def typedUseCase(useCase: UseCase): Unit  // !!!    don't return Unit!

    **/
  }
}
