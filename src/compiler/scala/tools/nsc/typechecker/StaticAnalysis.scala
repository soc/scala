/* NSC -- new Scala compiler
 * Copyright 2006-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package typechecker

import scala.collection.{ mutable, immutable }
import symtab.Flags._

trait StaticAnalysis {
  val global: Global

  import global._
  import definitions._

  // val closedWorld = sys.props contains "scalac.closed-world"
  val unusedOpts: Set[String] = if (sys.props contains "scalac.unused") sys.props("scalac.unused").split(',').toSet else Set()
  private def patternVars = unusedOpts("verbose")
  private def closedWorld = unusedOpts("closed")
  private def warnOnTypes = unusedOpts("types")

  object unusedConditions {
    // Amusing typing a result of needing TermName for implicit creation, but Name to use the Set on a Symbol's name
    private val serializationNames = Set[TermName]("readResolve", "readObject", "writeObject", "writeReplace").toSet[Name]
    private val AntTaskClass = rootMirror.getClassIfDefined("org.apache.tools.ant.Task")

    // Is the predicate true for this symbol or any symbol it overrides?
    def symbolOrAnyOverrideIs(m: Symbol)(p: Symbol => Boolean) = m.overrideChain exists (s => p(s.initialize))
    // Is the predicate true for this symbol or any symbol in its owner chain?
    def symbolOrAnyOwnerIs(m: Symbol)(p: Symbol => Boolean)    = m.ownerChain exists (s => p(s.initialize))
    // Is the predicate true for this symbol or any symbol among its declarations?
    def symbolOrAnyDeclIs(m: Symbol)(p: Symbol => Boolean)     = p(m) || (m.info.decls exists (s => p(s.initialize)))
    // Is the predicate true for this symbol or any symbol among its members?
    def symbolOrAnyMemberIs(m: Symbol)(p: Symbol => Boolean)   = p(m) || (m.info.members exists (s => p(s.initialize)))
    // Is the predicate true for this symbol or for its associated companion class, object, or module class?
    def symbolOrRelatedIs(m: Symbol)(p: Symbol => Boolean)   = {
      val syms = (
        if (m.isModuleClass) List(m, m.sourceModule, m.sourceModule.companionSymbol)
        else if (m.isModule) List(m, m.moduleClass, m.companionSymbol)
        else if (m.isClass)  List(m, m.companionSymbol, m.companionSymbol.moduleClass)
        else List(m)
      )
      syms filterNot (_ == NoSymbol) exists (s => p(s.initialize))
    }
    def symbolOrAnyOwnerOrRelatedIs(m: Symbol)(p: Symbol => Boolean) = (
         symbolOrRelatedIs(m)(p)
      || symbolOrAnyOwnerIs(m.owner)(p)
    )

    // These are frequently unused but necessary.
    def isSerializationMethod(m: Symbol) = serializationNames(m.name)
    // Finding methods by magic reflection names has to be the worst design imaginable.
    def isAntReflection(m: Symbol) = {
      def antSubclass = m.isClass && (m isSubClass AntTaskClass)
      def antMethod   = m.isMethod && (m.enclClass.typeOfThis <:< AntTaskClass.tpe)

      antSubclass || (
        antMethod && (m.info match {
          case MethodType(_ :: Nil, restpe) => restpe.typeSymbol == UnitClass
          case _                            => false
        })
      )
    }
    def excludeType(m: Symbol) = (
         m.isTypeParameterOrSkolem // TODO - exclude less here
      || m.isModuleClass           // check the module, not the module class
      || m.isOverride              // is leading us to false positives
    )
    def excludeTerm(m: Symbol) = (
         m.isMixinConstructor
      || (m.isConstructor && m.enclClass.isModuleClass)
      || m.isPackage
      || m.isParameter
      || m.isParamAccessor
      || m.accessedOrSelf.isEarlyInitialized        // lots of false positives in the way these are encoded
      || symbolOrAnyOverrideIs(m)(isUniversalMember) // methods which go back to Object - equals, toString, etc.
      || symbolOrAnyDeclIs(m)(hasConstantType)      // constant inlining can remove evidence a member was ever used
      || symbolOrAnyMemberIs(m)(isJavaMainMethod)   // main methods and containing objects are entry points
      || isSerializationMethod(m)
      || isAntReflection(m)
      // || (!m.isModule && !m.isMethod && !m.isPrivate && !m.isLocal) // visible var or val
    )

    def allowSynthetic(m: Symbol) = (
      m.name containsName nme.DEFAULT_GETTER_STRING
    )

    /** Tests for qualities for which we will exclude a member definition
     *  from consideration for reporting. Only half the story.
     */
    def excludeSymbol(m: Symbol): Boolean = (
         (m eq NoSymbol)
      || nme.isLocalName(m.name)
      || (m.name.toTermName == nme.WILDCARD)
      || symbolOrAnyOwnerOrRelatedIs(m)(s => (s hasFlag SYNTHETIC | ARTIFACT) && !allowSynthetic(s))
      || symbolOrAnyOverrideIs(m)(_.isJavaDefined)
      || symbolOrAnyOverrideIs(m)(_.isDeferred) // Exclude these for now, they are difficult to report usefully. TODO - tighten this
      || m.isTerm && excludeTerm(m)
      || m.isType && excludeType(m)
    )

    private def hasConstantType(m: Symbol) = isConstantType(m.info.resultType)

    def warnUnused(unit: CompilationUnit, defn: Tree): Unit = {
      val m = defn.symbol
      def decls = if (m.isClass) m.info.decls.toList else Nil
      def isDefaultGetter = m.name containsName nme.DEFAULT_GETTER_STRING

      val pos = (
        if (defn.pos.isDefined) defn.pos
        else if (m.pos.isDefined) m.pos
        else m match {
          case m: TermSymbol => m.referenced.pos
          case _             => NoPosition
        }
      )
      val why = (
        if (isDefaultGetter) ""
        else if (m.isPrivate) "private "
        else if (m.isLocal) "local "
        else if (closedWorld) "assuming a closed world, "
        else ""
      )
      val what = (
        if (m.isType) m.kindString
        else if (isDefaultGetter) "default argument"
        else if (m.isConstructor) "constructor"
        else if (m.isVal || m.isGetter && m.accessed.isVal) "val"
        else if (m.isVar || m.isGetter && m.accessed.isVar) "var"
        else if (m.isMethod) "method"
        else if (m.isModule) "object"
        else "term"
      )
      val also = if (decls.isEmpty) "" else s" (with ${decls.size} decls)"

      unit.warning(pos, s"$why$what$also is never used")
    }
  }
  import unusedConditions._

  trait UnusedSomewhere {
    def defns: List[(MemberDef, CompilationUnit)]
    def usedSyms: collection.Set[Symbol]
    def accesses: collection.Map[Symbol, List[Symbol]]

    def isUsed(m: Symbol)   = usedSyms(m)
    def isUnused(m: Symbol) = !isUsed(m)
    def isGot(m: Symbol)    = accesses(m) exists (s => (s eq m) || s.isGetter)
    def isSet(m: Symbol)    = accesses(m) exists (s => (s eq m) || s.isSetter)

    def check() {
      for ((defn, unit) <- defns) {
        val m  = defn.symbol
        val m1 = m.accessedOrSelf
        println(s"m/m1 $m $m1 ${accesses.getOrElse(m1, Nil)}")
        val isVar = (
             m1.isMutable && m1.isLocal
          || m1.isVar && !m1.isLazy
        )
        if (isVar && isGot(m1) && !isSet(m1)) // FIXME - isLazy is workaround for .isVar bug
          unit.warning(defn.pos, s"var ${m.name} in ${m.owner} is never set - it could be a val")
        else if (isUnused(m) && !m.isSetter)
          warnUnused(unit, defn)
      }
    }
  }

  private class UnusedInUnit(val unit: CompilationUnit) extends UnusedSomewhere {
    val defnTrees = mutable.ListBuffer[MemberDef]()
    val usedSyms  = mutable.Set[Symbol]()
    val accesses  = mutable.Map[Symbol, List[Symbol]]() withDefaultValue Nil

    def defns = defnTrees.toList map (_ -> unit)

    private object traverser extends Traverser {
      // Only record type references which don't originate within the
      // definition of the class being referenced.
      def isWithinDefinition(sym: Symbol): Boolean = currentOwner.ownerChain contains sym

      // critical to initialize symbols before performing flag-based tests
      def definitionQualifies(defn: Tree) = {
        val m = defn.symbol

        (m ne null) && !excludeSymbol(m.initialize) && (
             m.isPrivate
          || m.isLocal
          || (closedWorld && !m.owner.isPackageClass)
        )
      }
      def add(sym: Symbol) {
        println(s"[add] $sym ${sym.initialize.debugFlagString}")
        if ((sym != null) && (sym != NoSymbol) && !isWithinDefinition(sym.initialize)) {
          usedSyms += sym
          if (sym.isMutable && sym.isLocal) {
            accesses(sym) ::= sym
          }
          else if (sym.isGetter || sym.isSetter) {
            println(s"getter/setter: $sym/${sym.debugFlagString} ${sym.accessed}/${sym.accessed.debugFlagString}")
            accesses(sym.accessed) ::= sym
          }
        }
      }
      private var labelStack: List[LabelDef] = Nil
      private def inLabel[T](label: LabelDef)(body: => T): T = {
        labelStack ::= label
        try body
        finally labelStack = labelStack.tail
      }
      def addMemberDef(t: MemberDef) {
        def isPatternVar = labelStack.nonEmpty && t.symbol.isVal
        def shouldAdd    = definitionQualifies(t) && (patternVars || !isPatternVar)

        if (shouldAdd) {
          println(s"[defn] ${t.symbol} ${t.symbol.initialize.debugFlagString}")
          defnTrees += t
        }
      }
      override def traverse(t: Tree): Unit = {
        t match {
          case t: LabelDef      => inLabel(t)(super.traverse(t)) ; return
          case t: MemberDef     => addMemberDef(t)
          case t: RefTree       => t.symbol.overrideChain foreach add
          case Assign(lhs, rhs) =>
            lhs.symbol.initialize
            lhs.symbol.accessedOrSelf.initialize
            println(s"lhs=${lhs.symbol} ${lhs.symbol.debugFlagString}")
            // accesses(lhs.symbol.accessedOrSelf) ::= lhs.symbol
            // add(lhs.symbol)
            add(rhs.symbol)
          case _                                    =>
        }
        if (t.tpe ne null)
          t.tpe foreach (t1 => add(t1.typeSymbolDirect))

        super.traverse(t)
      }
    }

    locally {
      traverser traverse unit.body
    }
  }

  class UnusedInRun(units: List[CompilationUnit]) extends UnusedSomewhere {
    private val xs = units map (unit => new UnusedInUnit(unit))
    val usedSyms   = xs flatMap (_.usedSyms) toSet
    val defns      = xs flatMap (x => x.defnTrees.toList map (_ -> x.unit))
    val accesses   = xs.flatMap (_.accesses.toList).toMap withDefaultValue Nil
  }

  def checkUnused(units: List[CompilationUnit]) {
    if (closedWorld) new UnusedInRun(units) check()
    else units foreach (unit => new UnusedInUnit(unit) check())
  }
}
