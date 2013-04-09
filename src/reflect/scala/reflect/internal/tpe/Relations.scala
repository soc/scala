package scala.reflect
package internal
package tpe

import scala.collection.{ mutable }
import Flags._
import util.Statistics
import scala.annotation.tailrec

trait Relations {
  self: SymbolTable =>

  import definitions._
  import TypesStats._



  trait TypeRelation {
    def relateConstants(const1: Constant, const2: Constant): Boolean
    def relateMethods(params1: List[Symbol], res1: Type, params2: List[Symbol], res2: Type): Boolean
    def relatePolys(tparams1: List[Symbol], res1: Type, tparams2: List[Symbol], res2: Type): Boolean
    def relateBounds(lo1: Type, hi1: Type, lo2: Type, hi2: Type): Boolean
    def relateRefineds(parents1: List[Type], decls1: Scope, parents2: List[Type], decls2: Scope): Boolean
    def relateTypeRefs(tp1: TypeRef, tp2: TypeRef): Boolean
    def relateTypeRefOnLeft(tp1: TypeRef, tp2: Type): Boolean
    def relateTypeRefOnRight(tp1: Type, tp2: TypeRef): Boolean
    def relateSingletons(tp1: SingletonType, tp2: SingletonType): Boolean
    def relateOthers(tp1: Type, tp2: Type): Boolean
    def relate(tp1: Type, tp2: Type): Boolean
  }

  abstract class AbsTypeRelation extends TypeRelation {
    def relateConstants(const1: Constant, const2: Constant)                                      = c1 == c2
    def relateMethods(params1: List[Symbol], res1: Type, params2: List[Symbol], res2: Type)      =
    def relatePolys(tparams1: List[Symbol], res1: Type, tparams2: List[Symbol], res2: Type)      =
    def relateBounds(lo1: Type, hi1: Type, lo2: Type, hi2: Type)                                 =
    def relateRefineds(parents1: List[Type], decls1: Scope, parents2: List[Type], decls2: Scope) =
    def relateTypeRefs(tp1: TypeRef, tp2: TypeRef)                                               =
    def relateTypeRefOnLeft(tp1: TypeRef, tp2: Type)                                             =
    def relateTypeRefOnRight(tp1: Type, tp2: TypeRef)                                            =
    def relateSingletons(tp1: SingletonType, tp2: SingletonType)                                 =
    def relateOthers(tp1: Type, tp2: Type)                                                       =

    def apply(tp1: Type, tp2: Type): Boolean = relate(tp1, tp2)
    def relate(tp1: Type, tp2: Type): Boolean = tp1 match {
      case NullaryMethodType(res1)         => tp2 match { case NullaryMethodType(res2)         => relatee(res1, res2)                         ; case _ => false }
      case MethodType(params1, res1)       => tp2 match { case MethodType(params2, res2)       => relateMethods(params1, res1, params2, res2) ; case _ => false }
      case PolyType(tparams1, res1)        => tp2 match { case PolyType(tparams2, res2)        => relatePolys(tparams1, res1, tparams2, res2) ; case _ => false }
      case ExistentialType(tparams1, res1) => tp2 match { case ExistentialType(tparams2, res2) => relatePolys(tparams1, res1, tparams2, res2) ; case _ => false }
      case RefinedType(ps1, decls1)        => tp2 match { case RefinedType(ps2, decls2)        => relateRefineds(ps1, decls1, ps2, decls2)    ; case _ => false }
      case ConstantType(const1)            => tp2 match { case ConstantType(const2)            => relateConstants(const1, const2)             ; case _ => false }
      case TypeBounds(lo1, hi1)            => tp2 match { case TypeBounds(lo2, hi2)            => relateBounds(lo1, hi1, lo2, hi2)            ; case _ => false }
      case _                               => typeRefDispatch(tp1, tp2)
    }

    protected def typeRefDispatch(tp1: Type, tp2: Type): Boolean = tp1 match {
      case tp1: TypeRef =>
        tp2 match {
          case tp2: TypeRef => relateTypeRefs(tp1, tp2)
          case _            => relateTypeRefOnLeft(tp1, tp2)
        }
      case tp1: SingletonType =>
        tp2 match {
          case tp2: SingletonType => relateSingletons(tp1, tp2)
          case tp2: TypeRef       => relateTypeRefOnRight(tp1, tp2)
          case _                  => relateOthers(tp1, tp2)
        }
      case _ =>
        tp2 match {
          case tp2: TypeRef => relateTypeRefOnRight(tp1, tp2)
          case _            => relateOthers(tp1, tp2)
        }
    }

    def applyMethodTypes(tp1: MethodType, tp2: MethodType): Boolean = {
      def result1 = tp1.resultType
      def result2 = tp2.resultType.substSym(tp2.params, tp1.params)

      (    tp1.isImplicit == tp2.isImplicit
        && isSameTypes(tp1.paramTypes, tp2.paramTypes)
        && apply(result1, result2)
      )
    }
    def applyPolyTypes(tp1: PolyType, tp2: PolyType): Boolean = {
      val PolyType(tparams1, res1) = tp1
      val PolyType(tparams2, res2) = tp2
      // corresponds does not check length of two sequences before checking the predicate,
      // but SubstMap assumes it has been checked (SI-2956)
      sameLength(tparams1, tparams2) && {
        // fast-path: polymorphic method type -- type params cannot be captured
        val isMethod = tparams1.head.owner.isMethod
        //@M for an example of why we need to generate fresh symbols otherwise, see neg/tcpoly_ticket2101.scala
        val sharedTypeParams = if (isMethod) tparams1 else cloneSymbols(tparams1)

        def sub1(tp: Type)              = if (isMethod) tp else tp.substSym(tparams1, sharedTypeParams)
        def sub2(tp: Type)              = tp.substSym(tparams2, sharedTypeParams)
        def cmp(p1: Symbol, p2: Symbol) = sub2(p2.info) <:< sub1(p1.info)

        (tparams1 corresponds tparams2)(cmp) && apply(sub1(res1), sub2(res2))
      }
    }
  }

  object Conformance extends TypeRelation {
    def apply(tp1: Type, tp2: Type) = tp1 <:< tp2
  }
  object Equivalence extends TypeRelation {
    def apply(tp1: Type, tp2: Type) = tp1 =:= tp2
  }
  object JavaEquivalence extends TypeRelation {
    def apply(tp1: Type, tp2: Type) = tp1 =:= tp2
  }
  object Matching extends TypeRelation {
    def apply(tp1: Type, tp2: Type) = matchesType(tp1, tp2, alwaysMatchSimple = false)
  }
  object MatchingWithAlwaysOnSimple extends TypeRelation {
    def apply(tp1: Type, tp2: Type) = matchesType(tp1, tp2, alwaysMatchSimple = true)
  }


  private def isSameMethodType(mt1: MethodType, mt2: MethodType) = (
       isSameTypes(mt1.paramTypes, mt2.paramTypes)
    && (mt1.resultType =:= mt2.resultType.substSym(mt2.params, mt1.params))
    && (mt1.isImplicit == mt2.isImplicit)
  )

  private def equalTypeParamsAndResult(tparams1: List[Symbol], res1: Type, tparams2: List[Symbol], res2: Type) = {
    def subst(info: Type) = info.substSym(tparams2, tparams1)
    (    isSameTypes(tp1.paramTypes, tp2.paramTypes)
      && tp1.isImplicit == tp2.isImplicit
      && resOk
    )
  }

  private def isSameOrSubPolyType(tparams1: List[Symbol], res1: Type, tparams2: List[Symbol], res2: Type, same: Boolean): Boolean = (
    (    isSameTypes(tp1.paramTypes, tp2.paramTypes)
      && tp1.isImplicit == tp2.isImplicit
      && resOk
    )
  }

  private def isSameOrSubPolyType(tparams1: List[Symbol], res1: Type, tparams2: List[Symbol], res2: Type, same: Boolean): Boolean = (
    // corresponds does not check length of two sequences before checking the predicate,
    // but SubstMap assumes it has been checked (SI-2956)
    (     sameLength(tparams1, tparams2)
      && (tparams1 corresponds tparams2)((p1, p2) => p1.info =:= subst(p2.info))
      && (res1 =:= subst(res2))
    )
  }


  def matchesQuantified(tparams1: List[Symbol], res1: Type, tparams2: List[Symbol], res2: Type): Boolean = (
    (    sameLength(tparams1, tparams2)
      mt1.isImplicit == mt2.isImplicit &&
      matchingParams(params1, params2, mt1.isJava, mt2.isJava) &&

      && matchesType(res1, res2.substSym(tparams2, tparams1), alwaysMatchSimple)
    )
  )

}
