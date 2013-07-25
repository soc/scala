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

  private def generateProxy(clazz: Symbol): String = {
    val classInfo = clazz.tpe_*
    val syms = classInfo.members filter (m => m.isTerm && m.isOverridableMember && m.isPublic)
    val defs = syms flatMap { m =>
      val info      = classInfo memberType m
      val signature = (m defStringSeenAs info).replaceAll("\\w+\\.this\\.", "")
      val body      = info match {
        case NullaryMethodType(_)           => "wrapped." + m.decodedName
        case MethodType(ps, _)              => "wrapped." + m.decodedName + ps.map(_.decodedName).mkString("(", ", ", ")")
        case PolyType(_, MethodType(ps, _)) => "wrapped." + m.decodedName + ps.map(_.decodedName).mkString("(", ", ", ")")
        case _                              => ""
      }
      if (body == "") None else Some(s"    override $signature = $body")
    }

    defs mkString "\n"
  }

  class TypeProxy[T <: Type](wrapped: T) extends Type {

    override def isComplete: Boolean             = wrapped.isComplete
    override def isDependentMethodType: Boolean  = wrapped.isDependentMethodType
    override def isErroneous: Boolean            = wrapped.isErroneous
    override def isError: Boolean                = wrapped.isError
    override def isFinalType: Boolean            = wrapped.isFinalType
    override def isGround: Boolean               = wrapped.isGround
    override def isHigherKinded: Boolean         = wrapped.isHigherKinded
    override def isImmediatelyDependent: Boolean = wrapped.isImmediatelyDependent
    override def isSpliceable: Boolean           = wrapped.isSpliceable
    override def isStable: Boolean               = wrapped.isStable
    override def isStructuralRefinement: Boolean = wrapped.isStructuralRefinement
    override def isTrivial: Boolean              = wrapped.isTrivial
    override def isVolatile: Boolean             = wrapped.isVolatile
    override def isWildcard: Boolean             = wrapped.isWildcard

    override def exists(p: Type => Boolean): Boolean = wrapped.exists(p)
    override def filterAnnotations(p: AnnotationInfo => Boolean): Type = wrapped.filterAnnotations(p)
    override def find(p: Type => Boolean): Option[Type] = wrapped.find(p)
    override def foreach(f: Type => Unit): Unit = wrapped.foreach(f)
    override def map(f: Type => Type): Type = wrapped.map(f)
    override def <:<(that: Type): Boolean = wrapped.<:<(that)
    override def stat_<:<(that: Type): Boolean = wrapped.stat_<:<(that)
    override def weak_<:<(that: Type): Boolean = wrapped.weak_<:<(that)
    override def =:=(that: Type): Boolean = wrapped.=:=(that)

    override def findMember(name: Name,excludedFlags: Long,requiredFlags: Long,stableOnly: Boolean): Symbol =
      wrapped.findMember(name, excludedFlags, requiredFlags, stableOnly)

    override def findMembers(excludedFlags: Long,requiredFlags: Long): Scope =
      wrapped.findMembers(excludedFlags, requiredFlags)

    override def nonPrivateMembersAdmitting(admit: Long): Scope = wrapped.nonPrivateMembersAdmitting(admit)
    override def nonPrivateMemberAdmitting(name: Name,admit: Long): Symbol = wrapped.nonPrivateMemberAdmitting(name, admit)
    override def instantiateTypeParams(formals: List[Symbol],actuals: List[Type]): Type = wrapped.instantiateTypeParams(formals, actuals)
    override def memberBasedOnName(name: Name,excludedFlags: Long): Symbol = wrapped.memberBasedOnName(name, excludedFlags)
    override def membersBasedOnFlags(excludedFlags: Long,requiredFlags: Long): Scope = wrapped.membersBasedOnFlags(excludedFlags, requiredFlags)
    override def skolemizeExistential(owner: Symbol,origin: AnyRef): Type = wrapped.skolemizeExistential(owner, origin)
    override def substThisAndSym(from: Symbol,to: Type,symsFrom: List[Symbol],symsTo: List[Symbol]): Type = wrapped.substThisAndSym(from, to, symsFrom, symsTo)
    override def substituteSymbols(from: List[Symbol],to: List[Symbol]): Type = wrapped.substituteSymbols(from, to)
    override def substituteTypes(from: List[Symbol],to: List[Type]): Type = wrapped.substituteTypes(from, to)


    override def contains(sym: Symbol): Boolean                      = wrapped.contains(sym)
    override def equals(x$1: Any): Boolean                           = wrapped.equals(x$1)
    override def hasAnnotation(cls: Symbol): Boolean                 = wrapped.hasAnnotation(cls)
    override def looselyMatches(that: Type): Boolean                 = wrapped.looselyMatches(that)
    override def matches(that: Type): Boolean                        = wrapped.matches(that)
    override def matchesPattern(that: Type): Boolean                 = wrapped.matchesPattern(that)
    override def takesTypeArgs: Boolean                              = wrapped.takesTypeArgs
    override def addThrowsAnnotation(throwableSym: Symbol): Type     = wrapped.addThrowsAnnotation(throwableSym)
    override def annotations: List[AnnotationInfo]                   = wrapped.annotations
    override def asSeenFrom(pre: Type,clazz: Symbol): Type           = wrapped.asSeenFrom(pre, clazz)
    override def atOwner(owner: Symbol): Type                        = wrapped.atOwner(owner)
    override def baseClasses: List[Symbol]                           = wrapped.baseClasses
    override def baseType(clazz: Symbol): Type                       = wrapped.baseType(clazz)
    override def baseTypeIndex(sym: Symbol): Int                     = wrapped.baseTypeIndex(sym)
    override def baseTypeSeq: BaseTypeSeq                            = wrapped.baseTypeSeq
    override def baseTypeSeqDepth: Int                               = wrapped.baseTypeSeqDepth
    override def betaReduce: Type                                    = wrapped.betaReduce
    override def boundSyms: scala.collection.immutable.Set[Symbol]   = wrapped.boundSyms
    override def bounds: TypeBounds                                  = wrapped.bounds
    override def cloneInfo(owner: Symbol): Type                      = wrapped.cloneInfo(owner)
    override def collect[T](pf: PartialFunction[Type,T]): List[T]    = wrapped.collect(pf)
    override def complete(sym: Symbol): Unit                         = wrapped.complete(sym)
    override def computeMemberType(sym: Symbol): Type                = wrapped.computeMemberType(sym)
    override def dealias: Type                                       = wrapped.dealias
    override def dealiasWiden: Type                                  = wrapped.dealiasWiden
    override def dealiasWidenChain: List[Type]                       = wrapped.dealiasWidenChain
    override def decl(name: Name): Symbol                            = wrapped.decl(name)
    override def declaration(name: Name): Symbol                     = wrapped.declaration(name)
    override def declarations: Scope                                 = wrapped.declarations
    override def decls: Scope                                        = wrapped.decls
    override def deconst: Type                                       = wrapped.deconst
    override def deferredMembers: Scope                              = wrapped.deferredMembers
    override def directObjectString: String                          = wrapped.directObjectString
    override def erasure: Type                                       = wrapped.erasure
    override def etaExpand: Type                                     = wrapped.etaExpand
    override def finalResultType: Type                               = wrapped.finalResultType
    override def firstParent: Type                                   = wrapped.firstParent
    override def getAnnotation(cls: Symbol): Option[AnnotationInfo]  = wrapped.getAnnotation(cls)
    override def hashCode(): Int                                     = wrapped.hashCode()
    override def implicitMembers: Scope                              = wrapped.implicitMembers
    override def kind: String                                        = wrapped.kind
    override def load(sym: Symbol): Unit                             = wrapped.load(sym)
    override def member(name: Name): Symbol                          = wrapped.member(name)
    override def memberInfo(sym: Symbol): Type                       = wrapped.memberInfo(sym)
    override def memberType(sym: Symbol): Type                       = wrapped.memberType(sym)
    override def members: Scope                                      = wrapped.members
    override def narrow: Type                                        = wrapped.narrow
    override def nonLocalMember(name: Name): Symbol                  = wrapped.nonLocalMember(name)
    override def nonPrivateDecl(name: Name): Symbol                  = wrapped.nonPrivateDecl(name)
    override def nonPrivateDecls: List[Symbol]                       = wrapped.nonPrivateDecls
    override def nonPrivateMember(name: Name): Symbol                = wrapped.nonPrivateMember(name)
    override def nonPrivateMembers: Scope                            = wrapped.nonPrivateMembers
    override def normalize: Type                                     = wrapped.normalize
    override def paramSectionCount: Int                              = wrapped.paramSectionCount
    override def paramTypes: List[Type]                              = wrapped.paramTypes
    override def params: List[Symbol]                                = wrapped.params
    override def paramss: List[List[Symbol]]                         = wrapped.paramss
    override def parents: List[Type]                                 = wrapped.parents
    override def prefix: Type                                        = wrapped.prefix
    override def prefixChain: List[Type]                             = wrapped.prefixChain
    override def prefixString: String                                = wrapped.prefixString
    override def removeAnnotation(cls: Symbol): Type                 = wrapped.removeAnnotation(cls)
    override def resultApprox: Type                                  = wrapped.resultApprox
    override def resultType(actuals: List[Type]): Type               = wrapped.resultType(actuals)
    override def resultType: Type                                    = wrapped.resultType
    override def safeToString: String                                = wrapped.safeToString
    override def selfsym: Symbol                                     = wrapped.selfsym
    override def setAnnotations(annots: List[AnnotationInfo]): Type  = wrapped.setAnnotations(annots)
    override def skolemizeExistential: Type                          = wrapped.skolemizeExistential
    override def skolemsExceptMethodTypeParams: List[Symbol]         = wrapped.skolemsExceptMethodTypeParams
    override def subst(from: List[Symbol],to: List[Type]): Type      = wrapped.subst(from, to)
    override def substSym(from: List[Symbol],to: List[Symbol]): Type = wrapped.substSym(from, to)
    override def substThis(from: Symbol,to: Symbol): Type            = wrapped.substThis(from, to)
    override def substThis(from: Symbol,to: Type): Type              = wrapped.substThis(from, to)
    override def termSymbol: Symbol                                  = wrapped.termSymbol
    override def termSymbolDirect: Symbol                            = wrapped.termSymbolDirect
    override def throwsAnnotations(): List[Symbol]                   = wrapped.throwsAnnotations()
    override def toLongString: String                                = wrapped.toLongString
    override def trimPrefix(str: String): String                     = wrapped.trimPrefix(str)
    override def typeArgs: List[Type]                                = wrapped.typeArgs
    override def typeArguments: List[Type]                           = wrapped.typeArguments
    override def typeConstructor: Type                               = wrapped.typeConstructor
    override def typeOfThis: Type                                    = wrapped.typeOfThis
    override def typeParams: List[Symbol]                            = wrapped.typeParams
    override def typeSymbol: Symbol                                  = wrapped.typeSymbol
    override def typeSymbolDirect: Symbol                            = wrapped.typeSymbolDirect
    override def underlying: Type                                    = wrapped.underlying
    override def widen: Type                                         = wrapped.widen
    override def withAnnotations(annots: List[AnnotationInfo]): Type = wrapped.withAnnotations(annots)
    override def withSelfsym(sym: Symbol): Type                      = wrapped.withSelfsym(sym)
    override def withoutAnnotations: Type                            = wrapped.withoutAnnotations
    override def toString(): String                                  = wrapped.toString()
  }
}
