package scala
package reflect
package internal
package tpe

import scala.collection.{ mutable }
import util.Statistics
import scala.annotation.tailrec

trait TypeProxies {
  val global: SymbolTable
  import global._
  import definitions._

  // class TypeProxy[T <: Type](underlying: T) extends Type {
  trait TypeProxy extends Type {
    def <:<(that: Type): Boolean
    def =:=(that: Type): Boolean
    def addThrowsAnnotation(throwableSym: Symbol): Self
    def annotations: List[AnnotationInfo]
    def asSeenFrom(pre: Type,clazz: Symbol): Type
    def atOwner(owner: Symbol): Type
    def baseClasses: List[Symbol]
    def baseType(clazz: Symbol): Type
    def baseTypeIndex(sym: Symbol): Int
    def baseTypeSeq: BaseTypeSeq
    def baseTypeSeqDepth: Int
    def betaReduce: Type
    def boundSyms: Set[Symbol]
    def bounds: TypeBounds
    def cloneInfo(owner: Symbol): Type
    def collect[T](pf: PartialFunction[Type,T]): List[T]
    def complete(sym: Symbol): Unit
    def computeMemberType(sym: Symbol): Type
    def contains(sym: Symbol): Boolean
    def dealias: Type
    def dealiasWiden: Type
    def dealiasWidenChain: List[Type]
    def decl(name: Name): Symbol
    def declaration(name: Name): Symbol
    def declarations: Scope
    def decls: Scope
    def deconst: Type
    def deferredMembers: Scope
    def directObjectString: String
    def equals(x$1: Any): Boolean
    def erasure: Type
    def etaExpand: Type
    def exists(p: Type => Boolean): Boolean
    def filterAnnotations(p: AnnotationInfo => Boolean): Type
    def finalResultType: Type
    def find(p: Type => Boolean): Option[Type]
    def findMember(name: Name,excludedFlags: Long,requiredFlags: Long,stableOnly: Boolean): Symbol
    def findMembers(excludedFlags: Long,requiredFlags: Long): Scope
    def firstParent: Type
    def foreach(f: Type => Unit): Unit
    def getAnnotation(cls: Symbol): Option[AnnotationInfo]
    def hasAnnotation(cls: Symbol): Boolean
    def hashCode(): Int
    def implicitMembers: Scope
    def instantiateTypeParams(formals: List[Symbol],actuals: List[Type]): Type
    def isComplete: Boolean
    def isDependentMethodType: Boolean
    def isErroneous: Boolean
    def isError: Boolean
    def isFinalType: Boolean
    def isGround: Boolean
    def isHigherKinded: Boolean
    def isImmediatelyDependent: Boolean
    def isSpliceable: Boolean
    def isStable: Boolean
    def isStructuralRefinement: Boolean
    def isTrivial: Boolean
    def isVolatile: Boolean
    def isWildcard: Boolean
    def kind: String
    def load(sym: Symbol): Unit
    def looselyMatches(that: Type): Boolean
    def map(f: Type => Type): Type
    def matches(that: Type): Boolean
    def matchesPattern(that: Type): Boolean
    def member(name: Name): Symbol
    def memberBasedOnName(name: Name,excludedFlags: Long): Symbol
    def memberInfo(sym: Symbol): Type
    def memberType(sym: Symbol): Type
    def members: Scope
    def membersBasedOnFlags(excludedFlags: Long,requiredFlags: Long): Scope
    def narrow: Type
    def nonLocalMember(name: Name): Symbol
    def nonPrivateDecl(name: Name): Symbol
    def nonPrivateDecls: List[Symbol]
    def nonPrivateMember(name: Name): Symbol
    def nonPrivateMemberAdmitting(name: Name,admit: Long): Symbol
    def nonPrivateMembers: Scope
    def nonPrivateMembersAdmitting(admit: Long): Scope
    def normalize: Type
    def paramSectionCount: Int
    def paramTypes: List[Type]
    def params: List[Symbol]
    def paramss: List[List[Symbol]]
    def parents: List[Type]
    def prefix: Type
    def prefixChain: List[Type]
    def prefixString: String
    def removeAnnotation(cls: Symbol): Self
    def resultApprox: Type
    def resultType(actuals: List[Type]): Type
    def resultType: Type
    def safeToString: String
    def selfsym: Symbol
    def setAnnotations(annots: List[AnnotationInfo]): Type
    def skolemizeExistential(owner: Symbol,origin: AnyRef): Type
    def skolemizeExistential: Type
    def skolemsExceptMethodTypeParams: List[Symbol]
    def stat_<:<(that: Type): Boolean
    def subst(from: List[Symbol],to: List[Type]): Type
    def substSym(from: List[Symbol],to: List[Symbol]): Type
    def substThis(from: Symbol,to: Symbol): Type
    def substThis(from: Symbol,to: Type): Type
    def substThisAndSym(from: Symbol,to: Type,symsFrom: List[Symbol],symsTo: List[Symbol]): Type
    def substituteSymbols(from: List[Symbol],to: List[Symbol]): Type
    def substituteTypes(from: List[Symbol],to: List[Type]): Type
    def takesTypeArgs: Boolean
    def termSymbol: Symbol
    def termSymbolDirect: Symbol
    def throwsAnnotations(): List[Symbol]
    def toLongString: String
    def trimPrefix(str: String): String
    def typeArgs: List[Type]
    def typeArguments: List[Type]
    def typeConstructor: Type
    def typeOfThis: Type
    def typeParams: List[Symbol]
    def typeSymbol: Symbol
    def typeSymbolDirect: Symbol
    def underlying: Type
    def weak_<:<(that: Type): Boolean
    def widen: Type
    def withAnnotations(annots: List[AnnotationInfo]): Type
    def withFilter(p: Type => Boolean): FilterMapForeach
    def withSelfsym(sym: Symbol): Type
    def withoutAnnotations: Type
  }
}
