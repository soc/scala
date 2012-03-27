package scala.tools.nsc
package api

trait ScalaTyperErrors[Result] {
  def AbstractExistentiallyOverParamerizedTpeError(tree: Tree, tp: Type): Result
  def AbstractionFromVolatileTypeError(vd: ValDef): Result
  def AdaptTypeError(tree: Tree, found: Type, req: Type): Result
  def AmbiguousIdentError(tree: Tree, name: Name, msg: String): Result
  def AmbiguousParentClassError(tree: Tree): Result
  def AnnotationArgNullError(tree: Tree): Result
  def AnnotationMissingArgError(tree: Tree, annType: Type, sym: Symbol): Result
  def AnnotationNotAConstantError(tree: Tree): Result
  def AnnotationTypeMismatchError(tree: Tree, expected: Type, found: Type): Result
  def AppliedTypeNoParametersError(tree: Tree, errTpe: Type): Result
  def AppliedTypeWrongNumberOfArgsError(tree: Tree, tpt: Tree, tparams: List[Symbol]): Result
  def ApplyWithoutArgsError(tree: Tree, fun: Tree): Result
  def ArrayConstantsError(tree: Tree): Result
  def ArrayConstantsTypeMismatchError(tree: Tree, pt: Type): Result
  def AssignmentError(tree: Tree, varSym: Symbol): Result
  def CaseClassConstructorError(tree: Tree): Result
  def ClassTypeRequiredError(tree: Tree, found: AnyRef): Result
  def ClassfileAnnotationsAsNamedArgsError(tree: Tree): Result
  def ConstrArgsInTraitParentTpeError(arg: Tree, parent: Symbol): Result
  def ConstructorPrefixError(tree: Tree, restpe: Type): Result
  def ConstructorsOrderError(tree: Tree): Result
  def CyclicAliasingOrSubtypingError(errPos: Position, sym0: Symbol): Result
  def CyclicReferenceError(errPos: Position, lockedSym: Symbol): Result
  def DefDefinedTwiceError(sym0: Symbol, sym1: Symbol): Result
  def DependentMethodTpeConversionToFunctionError(tree: Tree, tp: Type): Result
  def DeprecatedParamNameError(param: Symbol, name: Name): Result
  def DoesNotConformToSelfTypeError(tree: Tree, sym: Symbol, tpe0: Type): Result
  def DuplicateValueAnnotationError(tree: Tree, name: Name): Result
  def FinalVolatileVarError(vdef: Tree): Result
  def FinitaryError(tparam: Symbol): Result
  def InferTypeWithVolatileTypeSelectionError(tree: Tree, pre: Type): Result
  def InvalidConstructorDefError(ddef: Tree): Result
  def IsAbstractError(tree: Tree, sym: Symbol): Result
  def KindArityMismatchError(tree: Tree, pt: Type): Result
  def LocalVarUninitializedError(vdef: Tree): Result
  def LowerBoundError(tree: TypeDef, lowB: Type, highB: Type): Result
  def MaxFunctionArityError(fun: Tree): Result
  def MissingArgsForMethodTpeError(tree: Tree, meth: Symbol): Result
  def MissingManifestError(tree: Tree, full: Boolean, tp: Type): Result
  def MissingParameterTypeAnonMatchError(vparam: Tree, pt: Type): Result
  def MissingParameterTypeError(fun: Tree, vparam: ValDef, pt: Type): Result
  def MissingTypeArgumentsParentTpeError(supertpt: Tree): Result
  def MissingTypeParametersError(tree: Tree): Result
  def MixinMissingParentClassNameError(tree: Tree, mix: Name, clazz: Symbol): Result
  def ModuleUsingCompanionClassDefaultArgsErrror(tree: Tree): Result
  def MultiDimensionalArrayError(tree: Tree): Result
  def MultipleArgumentListForAnnotationError(tree: Tree): Result
  def MultipleVarargError(tree: Tree): Result
  def NestedAnnotationError(tree: Tree, annType: Type): Result
  def NoImplicitFoundError(tree: Tree, param: Symbol): Result
  def NotAMemberError(sel: Tree, qual: Tree, name: Name): Result
  def NotAValueError(tree: Tree, sym: Symbol): Result
  def NotEnoughArgsError(tree: Tree, fun0: Tree, missing0: List[Symbol]): Result
  def OnlyDeclarationsError(tree: Tree): Result
  def OverloadedUnapplyError(tree: Tree): Result
  def ParentFinalInheritanceError(parent: Tree, mixin: Symbol): Result
  def ParentInheritedTwiceError(parent: Tree, parentSym: Symbol): Result
  def ParentNotATraitMixinError(parent: Tree, mixin: Symbol): Result
  def ParentSealedInheritanceError(parent: Tree, psym: Symbol): Result
  def ParentSelfTypeConformanceError(parent: Tree, selfType: Type): Result
  def ParentSuperSubclassError(parent: Tree, superclazz: Symbol, parentSym: Symbol, mixin: Symbol): Result
  def ParentTypesError(templ: Template, ex: TypeError): Result
  def QualifyingClassError(tree: Tree, qual: Name): Result
  def ReturnOutsideOfDefError(tree: Tree): Result
  def ReturnWithoutTypeError(tree: Tree, owner: Symbol): Result
  def StarParamNotLastError(param: Tree): Result
  def StarPatternWithVarargParametersError(tree: Tree): Result
  def StarPositionInPatternError(tree: Tree): Result
  def StarWithDefaultError(meth: Symbol): Result
  def SuperConstrArgsThisReferenceError(tree: Tree): Result
  def SuperConstrReferenceError(tree: Tree): Result
  def SymbolNotFoundError(tree: Tree, name: Name, owner: Symbol, startingIdentCx: Context): Result
  def TooManyArgsNamesDefaultsError(tree: Tree, fun: Tree): Result
  def TooManyArgsPatternError(fun: Tree): Result
  def TypeNotAStablePrefixError(tpt: Tree, pre: Type): Result
  def TypeSelectionFromVolatileTypeError(tree: Tree, qual: Tree): Result
  def TypedApplyDoesNotTakeTpeParametersError(tree: Tree, fun: Tree): Result
  def TypedApplyWrongNumberOfTpeParametersError(tree: Tree, fun: Tree): Result
  def UnapplyWithSingleArgError(tree: Tree): Result
  def UnderscoreEtaError(tree: Tree): Result
  def UnexpectedTreeAnnotation(tree: Tree): Result
  def UnexpectedTreeAnnotationError(tree: Tree, unexpected: Tree): Result
  def UnexpectedTreeAnnotationError(tree: Tree, unexpected: Tree): Result
  def UnexpectedTreeAssignmentConversionError(tree: Tree): Result
  def UnknownAnnotationNameError(tree: Tree, name: Name): Result
  def UnstableTreeError(tree: Tree): Result
  def VariableInPatternAlternativeError(tree: Tree): Result
  def VolatileValueError(vdef: Tree): Result
  def WithFilterError(tree: Tree, ex: AbsTypeError): Result
  def WrongNumberArgsPatternError(tree: Tree, fun: Tree): Result
  def AppliedTypeWrongNumberOfArgsError(tree: Tree, tpt: Tree, tparams: List[Symbol]): Result
  def WrongNumberOfArgsError(tree: Tree, fun: Tree): Result
  def WrongNumberOfParametersError(tree: Tree, argpts: List[Type]): Result
}
