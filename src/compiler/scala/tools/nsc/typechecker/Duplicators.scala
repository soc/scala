/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import scala.tools.nsc.symtab.Flags
import scala.collection.{ mutable, immutable }

/** Duplicate trees and re-type check them, taking care to replace
 *  and create fresh symbols for new local definitions.
 *
 *  @author  Iulian Dragos
 *  @version 1.0
 */
abstract class Duplicators extends Analyzer {
  import global._
  import definitions.{ AnyRefClass, AnyValClass }

  /** Retype the given tree in the given context. Use this method when retyping
   *  a method in a different class. The typer will replace references to the this of
   *  the old class with the new class, and map symbols through the given 'env'. The
   *  environment is a map from type skolems to concrete types (see SpecializedTypes).
   */
  def retyped(context: Context, tree: Tree, oldThis: Symbol, newThis: Symbol, env: scala.collection.Map[Symbol, Type]): Tree = {
    if (oldThis ne newThis) {
      oldClassOwner = oldThis
      newClassOwner = newThis
    } else resetClassOwners()

    envSubstitution = new SubstSkolemsTypeMap(env.keysIterator.toList, env.valuesIterator.toList)
    debuglog("retyped with env: " + env)
    newBodyDuplicator(context).typed(tree)
  }

  protected def newBodyDuplicator(context: Context) = new BodyDuplicator(context)

  /** Return the special typer for duplicate method bodies. */
  override def newTyper(context: Context): Typer =
    newBodyDuplicator(context)

  private def resetClassOwners() {
    oldClassOwner = null
    newClassOwner = null
  }

  private var oldClassOwner: Symbol = _
  private var newClassOwner: Symbol = _
  private var envSubstitution: SubstTypeMap = _

  private class SubstSkolemsTypeMap(from: List[Symbol], to: List[Type]) extends SubstTypeMap(from, to) {
    protected override def matches(sym1: Symbol, sym2: Symbol) =
      if (sym2.isTypeSkolem) sym2.deSkolemize eq sym1
      else sym1 eq sym2
  }

  private val invalidSyms: mutable.Map[Symbol, Tree] = perRunCaches.newMap[Symbol, Tree]()

  /** A typer that creates new symbols for all definitions in the given tree
   *  and updates references to them while re-typechecking. All types in the
   *  tree, except for TypeTrees, are erased prior to type checking. TypeTrees
   *  are fixed by substituting invalid symbols for the new ones.
   */
  class BodyDuplicator(_context: Context) extends Typer(_context) {

    class FixInvalidSyms extends TypeMap {
      def apply(tpe: Type): Type = mapOver(tpe)

      override def mapOver(tpe: Type): Type = tpe match {
        case TypeRef(NoPrefix, sym, args) if sym.isTypeParameterOrSkolem =>
          var sym1 = context.scope.lookup(sym.name)
          if (sym1 eq NoSymbol) {
            // try harder (look in outer scopes)
            // with virtpatmat, this can happen when the sym is referenced in the scope of a LabelDef but is defined in the scope of an outer DefDef (e.g., in AbstractPartialFunction's andThen)
            BodyDuplicator.super.silent(_.typedType(Ident(sym.name))) match {
              case SilentResultValue(t) =>
                sym1 = t.symbol
                debuglog("fixed by trying harder: "+((sym, sym1, context)))
              case _ =>
            }
          }
//          assert(sym1 ne NoSymbol, tpe)
          if ((sym1 ne NoSymbol) && (sym1 ne sym)) {
            debuglog("fixing " + sym + " -> " + sym1)
            typeRef(NoPrefix, sym1, mapOverArgs(args, sym1.typeParams))
          } else super.mapOver(tpe)

        case TypeRef(pre, sym, args) =>
          val newsym = updateSym(sym)
          if (newsym ne sym) {
            debuglog("fixing " + sym + " -> " + newsym)
            typeRef(mapOver(pre), newsym, mapOverArgs(args, newsym.typeParams))
          } else
            super.mapOver(tpe)

        case SingleType(pre, sym) =>
          val sym1 = updateSym(sym)
          if (sym1 ne sym) {
            debuglog("fixing " + sym + " -> " + sym1)
            singleType(mapOver(pre), sym1)
          } else
            super.mapOver(tpe)

        case ThisType(sym) =>
          val sym1 = updateSym(sym)
          if (sym1 ne sym) {
            debuglog("fixing " + sym + " -> " + sym1)
            ThisType(sym1)
          } else
            super.mapOver(tpe)


        case _ =>
          super.mapOver(tpe)
      }
    }

    /** Fix the given type by replacing invalid symbols with the new ones. */
    def fixType(tpe: Type): Type = {
      val tpe1 = new FixInvalidSyms apply envSubstitution(tpe)
      newClassOwner match {
        case null  => tpe1
        case owner => tpe1.asSeenFrom(owner.thisType, oldClassOwner)
      }
    }

    /** Return the new symbol corresponding to `sym`. */
    private def updateSym(sym: Symbol): Symbol =
      if (invalidSyms.isDefinedAt(sym))
        invalidSyms(sym).symbol
      else
        sym

    private def invalidate(tree: Tree) { invalidate(tree, NoSymbol) }
    private def invalidate(tree: Tree, owner: Symbol) {
      if (tree.isDef && tree.symbol != NoSymbol) {
        invalidSyms(tree.symbol) = tree

        tree match {
          case ldef @ LabelDef(name, params, rhs) =>
            invalidSyms(ldef.symbol) = ldef
            ldef.symbol = ldef.symbol cloneSymbol context.owner modifyInfo fixType

          case vdef @ ValDef(mods, name, _, rhs) if mods.isLazy =>
            invalidSyms(vdef.symbol) = vdef
            val clone = vdef.symbol cloneSymbol (owner orElse context.owner) modifyInfo fixType
            vdef.symbol = clone
            if (clone.owner.isClass)
              clone.owner.info.decls enter clone

          case DefDef(_, name, tparams, vparamss, _, rhs) =>
            // invalidate parameters
            tparams foreach (invalidate(_))
            mforeach(vparamss)(invalidate(_))
            tree.symbol = NoSymbol

          case _ =>
            tree.symbol = NoSymbol
        }
      }
    }

    /** Optionally cast this tree into some other type, if required.
     *  Unless overridden, just returns the tree.
     */
    def castType(tree: Tree, pt: Type): Tree = tree

    /** Special typer method for re-type checking trees. It expects a typed tree.
     *  Returns a typed tree that has fresh symbols for all definitions in the original tree.
     *
     *  Each definition tree is visited and its symbol added to the invalidSyms map (except LabelDefs),
     *  then cleared (forcing the namer to create fresh symbols).
     *  All invalid symbols found in trees are cleared (except for LabelDefs), forcing the
     *  typechecker to look for fresh ones in the context.
     *
     *  Type trees are typed by substituting old symbols for new ones (@see fixType).
     *
     *  LabelDefs are not typable from trees alone, unless they have the type ()Unit. Therefore,
     *  their symbols are recreated ad-hoc and their types are fixed inline, instead of letting the
     *  namer/typer handle them, or Idents that refer to them.
     */
    override def typed(tree: Tree, mode: Mode, pt: Type): Tree = {
      debuglog("typing " + tree + ": " + tree.tpe + ", " + tree.getClass)
      val origtreesym = tree.symbol
      if (tree.hasSymbolField && tree.symbol != NoSymbol
          && !tree.symbol.isLabel  // labels cannot be retyped by the type checker as LabelDef has no ValDef/return type trees
          && invalidSyms.isDefinedAt(tree.symbol)) {
        debuglog("removed symbol " + tree.symbol)
        tree.symbol = NoSymbol
      }

      def clearTypeAndRecurse(): Tree = super.typed(tree.clearType(), mode, pt)
      def castToPtAndRecurse(): Tree  = super.typed(castType(tree, pt), mode, pt)

      tree match {
        case ttree @ TypeTree() =>
          ttree modifyType fixType

        case Block(stats, res) =>
          stats :+ res foreach (invalidate(_))
          clearTypeAndRecurse()

        case ClassDef(_, _, _, tmpl @ Template(_, _, stats)) =>
          tmpl.symbol = tree.symbol.newLocalDummy(tree.pos)
          stats foreach (invalidate(_, tree.symbol))
          clearTypeAndRecurse()

        case defn: ValOrDefDef =>
          defn.tpt modifyType fixType
          clearTypeAndRecurse()

        case ldef @ LabelDef(name, params, rhs) =>
          // in case the rhs contains any definitions -- TODO: is this necessary?
          invalidate(rhs)
          ldef.clearType()

          // is this LabelDef generated by tailcalls?
          val isTailLabel = ldef.params match {
            case p :: _ :: _ => p.name == nme.THIS
            case _           => false
          }
          // the typer does not create the symbols for a LabelDef's params, so unless they were created before we need
          // to do it manually here -- but for the tailcalls-generated labels, ValDefs are created before the LabelDef,
          // so we just need to change the tree to point to the updated symbols
          def newParam(p: Tree): Ident = Ident(
            if (isTailLabel) updateSym(p.symbol)
            else p.symbol.cloneSymbol modifyInfo fixType // TODO context.owner, owner?
          )
          val params1 = params map newParam
          val rhs1 = new TreeSubstituter(params map (_.symbol), params1) transform rhs // TODO: duplicate?

          super.typed(treeCopy.LabelDef(tree, name, params1, rhs1.clearType()), mode, pt)

        case Bind(name, _) =>
          invalidate(tree)
          clearTypeAndRecurse()

        case Ident(_) if tree.symbol.isLabel =>
          tree.symbol = updateSym(tree.symbol)
          clearTypeAndRecurse()

        case Ident(_) if (origtreesym ne null) && origtreesym.isLazy =>
          tree.symbol = updateSym(origtreesym)
          clearTypeAndRecurse()

        case Select(th @ This(_), sel) if (oldClassOwner ne null) && (th.symbol == oldClassOwner) =>
          // We use the symbol name instead of the tree name because the symbol
          // may have been name mangled, rendering the tree name obsolete.
          // ...but you can't just do a Select on a name because if the symbol is
          // overloaded, you will crash in the backend.
          val memberByName  = newClassOwner.thisType.member(tree.symbol.name)
          def nameSelection = Select(This(newClassOwner), tree.symbol.name)
          val newTree = (
            if (memberByName.isOverloaded) {
              // Find the types of the overload alternatives as seen in the new class,
              // and filter the list down to those which match the old type (after
              // fixing the old type so it is seen as if from the new class.)
              val typeInNewClass = fixType(oldClassOwner.info memberType tree.symbol)
              val alts           = memberByName.alternatives
              val memberTypes    = alts map (newClassOwner.info memberType _)
              val memberString   = memberByName.defString
              alts zip memberTypes filter (_._2 =:= typeInNewClass) match {
                case ((alt, tpe)) :: Nil =>
                  log(s"Arrested overloaded type in Duplicators, narrowing to ${alt.defStringSeenAs(tpe)}\n  Overload was: $memberString")
                  Select(This(newClassOwner), alt)
                case xs =>
                  alts filter (alt => (alt.paramss corresponds tree.symbol.paramss)(_.size == _.size)) match {
                    case alt :: Nil =>
                      log(s"Resorted to parameter list arity to disambiguate to $alt\n  Overload was: $memberString")
                      Select(This(newClassOwner), alt)
                    case _ =>
                      log(s"Could not disambiguate $memberTypes. Attempting name-based selection, but we may crash later.")
                      nameSelection
                  }
              }
            }
            else nameSelection
          )
          super.typed(atPos(tree.pos)(newTree), mode, pt)

        case This(_) if (oldClassOwner ne null) && (tree.symbol == oldClassOwner) =>
          super.typedPos(tree.pos, mode, pt)(This(newClassOwner))

        case This(_) =>
          tree.symbol = updateSym(tree.symbol)
          castToPtAndRecurse()

        case Match(scrut, cases) =>
          val scrut1   = typed(scrut, EXPRmode | BYVALmode, WildcardType)
          val scrutTpe = scrut1.tpe.widen
          val cases1 = {
            if (scrutTpe.isFinalType) cases filter {
              case CaseDef(Bind(_, pat @ Typed(_, tpt)), EmptyTree, body) =>
                // the typed pattern is not incompatible with the scrutinee type
                scrutTpe matchesPattern fixType(tpt.tpe)
              case CaseDef(Typed(_, tpt), EmptyTree, body) =>
                // the typed pattern is not incompatible with the scrutinee type
                scrutTpe matchesPattern fixType(tpt.tpe)
              case _ => true
            }
            // Without this, AnyRef specializations crash on patterns like
            //   case _: Boolean => ...
            // Not at all sure this is safe.
            else if (scrutTpe <:< AnyRefClass.tpe)
              cases filterNot (_.pat.tpe <:< AnyValClass.tpe)
            else
              cases
          }

          super.typed(atPos(tree.pos)(Match(scrut, cases1)), mode, pt)

        case EmptyTree =>
          // no need to do anything, in particular, don't set the type to null, EmptyTree.tpe_= asserts
          tree

        case _ =>
          if (tree.hasSymbolField && tree.symbol != NoSymbol && (tree.symbol.owner == definitions.AnyClass)) {
            tree.symbol = NoSymbol // maybe we can find a more specific member in a subclass of Any (see AnyVal members, like ==)
          }
          castToPtAndRecurse()
      }
    }

  }
}

