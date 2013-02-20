/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import scala.collection.{ mutable, immutable }
import symtab.Flags._

trait Bridges extends ast.TreeDSL {
  import global._
  import definitions._
  import CODE._

  protected def bridgeErasure(sym: Symbol)(tp: Type): Type

  /**  Add bridge definitions to a template. This means:
   *
   *   If there is a concrete member `m` which overrides a member in a base
   *   class of the template, and the erased types of the two members differ,
   *   and the two members are not inherited or defined by some parent class
   *   of the template, then a bridge from the overridden member `m1` to the
   *   member `m0` is added. The bridge has the erased type of `m1` and
   *   forwards to `m0`.
   *
   *   No bridge is added if there is already a bridge to `m0` with the erased
   *   type of `m1` in the template.
   */
  def bridgeDefs(unit: CompilationUnit, owner: Symbol): (List[Tree], immutable.Set[Symbol]) = {
    assert(phase == currentRun.erasurePhase, phase)
    if (owner.isTrait)
      (Nil, Set())
    else {
      debuglog("computing bridges for " + owner)
      new ComputeBridges(unit, owner) compute()
    }
  }

  def addBridges(unit: CompilationUnit, stats: List[Tree], base: Symbol): List[Tree] = (
    bridgeDefs(unit, base) match {
      case (bridges, removals) if bridges.nonEmpty =>
        stats.filterNot(removals contains _.symbol) ::: bridges
      case _ =>
        stats
    }
  )

  private class ComputeBridges(unit: CompilationUnit, root: Symbol) {
    assert(phase == currentRun.erasurePhase, phase)

    var toBeRemoved  = immutable.Set[Symbol]()
    val site         = root.thisType
    val bridgesScope = newScope
    val bridgeTarget = mutable.HashMap[Symbol, Symbol]()
    var bridges      = List[Tree]()

    val deconstMap = new TypeMap {
      // For some reason classOf[Foo] creates ConstantType(Constant(tpe)) with an actual Type for tpe,
      // which is later translated to a Class. Unfortunately that means we have bugs like the erasure
      // of Class[Foo] and classOf[Bar] not being seen as equivalent, leading to duplicate method
      // generation and failing bytecode. See ticket #4753.
      def apply(tp: Type): Type = tp match {
        case PolyType(_, _)                  => mapOver(tp)
        case MethodType(_, _)                => mapOver(tp)     // nullarymethod was eliminated during uncurry
        case ConstantType(Constant(_: Type)) => ClassClass.tpe  // all classOfs erase to Class
        case _                               => tp.deconst
      }
    }

    val opc = enteringExplicitOuter {
      new overridingPairs.Cursor(root) {
        override def parents              = List(root.info.firstParent)
        override def exclude(sym: Symbol) = !sym.isMethod || sym.isPrivate || super.exclude(sym)
      }
    }

    def compute(): (List[Tree], immutable.Set[Symbol]) = {
      while (opc.hasNext) {
        val member = opc.overriding
        val other  = opc.overridden
        //println("bridge? " + member + ":" + member.tpe + member.locationString + " to " + other + ":" + other.tpe + other.locationString)//DEBUG
        if (enteringExplicitOuter(!member.isDeferred))
          checkPair(member, other)

        opc.next
      }
      (bridges, toBeRemoved)
    }

    /** Check that a bridge only overrides members that are also overridden by the original member.
     *  This test is necessary only for members that have a value class in their type.
     *  Such members are special because their types after erasure and after post-erasure differ/.
     *  This means we generate them after erasure, but the post-erasure transform might introduce
     *  a name clash. The present method guards against these name clashes.
     *
     *  @param  member   The original member
     *  @param  other    The overidden symbol for which the bridge was generated
     *  @param  bridge   The bridge
     */
    def checkBridgeOverrides(member: Symbol, other: Symbol, bridge: Symbol): Boolean = {
      def fulldef(sym: Symbol) =
        if (sym == NoSymbol) sym.toString
        else s"$sym: ${sym.tpe} in ${sym.owner}"
      var noclash = true
      def clashError(what: String) = {
        noclash = false
        unit.error(
          if (member.owner == root) member.pos else root.pos,
          sm"""bridge generated for member ${fulldef(member)}
              |which overrides ${fulldef(other)}
              |clashes with definition of $what;
              |both have erased type ${exitingPostErasure(bridge.tpe)}""")
      }
      for (bc <- root.baseClasses) {
        if (settings.debug.value)
          exitingPostErasure(println(
            sm"""check bridge overrides in $bc
                |${bc.info.nonPrivateDecl(bridge.name)}
                |${site.memberType(bridge)}
                |${site.memberType(bc.info.nonPrivateDecl(bridge.name) orElse IntClass)}
                |${(bridge.matchingSymbol(bc, site))}"""))

        def overriddenBy(sym: Symbol) =
          sym.matchingSymbol(bc, site).alternatives filter (sym => !sym.isBridge)
        for (overBridge <- exitingPostErasure(overriddenBy(bridge))) {
          if (overBridge == member) {
            clashError("the member itself")
          } else {
            val overMembers = overriddenBy(member)
            if (!overMembers.exists(overMember =>
              exitingPostErasure(overMember.tpe =:= overBridge.tpe))) {
              clashError(fulldef(overBridge))
            }
          }
        }
      }
      noclash
    }

    def checkPair(member: Symbol, other: Symbol) {
      val otpe = bridgeErasure(root)(other.tpe)
      val bridgeNeeded = exitingErasure (
        !(other.tpe =:= member.tpe) &&
        !(deconstMap(other.tpe) =:= deconstMap(member.tpe)) &&
        { var e = bridgesScope.lookupEntry(member.name)
          while ((e ne null) && !((e.sym.tpe =:= otpe) && (bridgeTarget(e.sym) == member)))
            e = bridgesScope.lookupNextEntry(e)
          (e eq null)
        }
      )
      if (!bridgeNeeded)
        return

      val newFlags = (member.flags | BRIDGE | ARTIFACT) & ~(ACCESSOR | DEFERRED | LAZY | lateDEFERRED)
      val bridge   = other.cloneSymbolImpl(root, newFlags) setPos root.pos

      debuglog("generating bridge from %s (%s): %s to %s: %s".format(
        other, flagsToString(newFlags),
        otpe + other.locationString, member,
        bridgeErasure(root)(member.tpe) + member.locationString)
      )

      // the parameter symbols need to have the new owner
      bridge setInfo (otpe cloneInfo bridge)
      bridgeTarget(bridge) = member

      if (!(member.tpe exists (_.typeSymbol.isDerivedValueClass)) ||
          checkBridgeOverrides(member, other, bridge)) {
        exitingErasure(root.info.decls enter bridge)
        if (other.owner == root) {
          exitingErasure(root.info.decls.unlink(other))
          toBeRemoved += other
        }

        bridgesScope enter bridge
        bridges ::= makeBridgeDefDef(bridge, member, other)
      }
    }

    def makeBridgeDefDef(bridge: Symbol, member: Symbol, other: Symbol) = exitingErasure {
      // type checking ensures we can safely call `other`, but unless `member.tpe <:< other.tpe`,
      // calling `member` is not guaranteed to succeed in general, there's
      // nothing we can do about this, except for an unapply: when this subtype test fails,
      // return None without calling `member`
      //
      // TODO: should we do this for user-defined unapplies as well?
      // does the first argument list have exactly one argument -- for user-defined unapplies we can't be sure
      def maybeWrap(bridgingCall: Tree): Tree = {
        val guardExtractor = ( // can't statically know which member is going to be selected, so don't let this depend on member.isSynthetic
             (member.name == nme.unapply || member.name == nme.unapplySeq)
          && !exitingErasure((member.tpe <:< other.tpe))) // no static guarantees (TODO: is the subtype test ever true?)

        import CODE._
        val _false    = FALSE
        val pt        = member.tpe.resultType
        lazy val zero =
          if      (_false.tpe <:< pt)    _false
          else if (NoneModule.tpe <:< pt) REF(NoneModule)
          else EmptyTree

        if (guardExtractor && (zero ne EmptyTree)) {
          val typeTest = gen.mkIsInstanceOf(REF(bridge.firstParam), member.tpe.params.head.tpe)
          IF (typeTest) THEN bridgingCall ELSE zero
        } else bridgingCall
      }
      val rhs = member.tpe match {
        case MethodType(Nil, ConstantType(c)) => Literal(c)
        case _                                =>
          val sel: Tree    = Select(This(root), member)
          val bridgingCall = (sel /: bridge.paramss)((fun, vparams) => Apply(fun, vparams map Ident))

          maybeWrap(bridgingCall)
      }
      atPos(bridge.pos)(DefDef(bridge, rhs))
    }
  }
}
