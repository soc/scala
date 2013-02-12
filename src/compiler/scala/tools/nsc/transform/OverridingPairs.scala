/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import scala.collection.mutable
import symtab.Flags._
import util.HashSet
import scala.annotation.tailrec

abstract class OverridingPairs extends SymbolPairs {
  import global._

  class Cursor(base: Symbol) extends super.Cursor(base) {
    lazy val relatively = new RelativeTo(base.thisType)

    /** Symbols to exclude: Here these are constructors and private/artifact symbols,
     *  including bridges. But it may be refined in subclasses.
     */
    override protected def exclude(sym: Symbol) = (
         (sym hasFlag PRIVATE | ARTIFACT)
      || sym.isConstructor
    )

    /** Types always match. Term symbols match if their member types
     *  relative to `self` match.
     */
    override protected def matches(sym1: Symbol, sym2: Symbol) = sym1.isType || (
         (sym1.owner != sym2.owner)
      && !exclude(sym2)
      && relatively.matches(sym1, sym2)
    )
  }
}

/** A class that yields a kind of iterator (`Cursor`),
 *  which yields pairs of corresponding symbols visible in some base class,
 *  unless there's a parent class that already contains the same pairs.
 *  @author Martin Odersky
 *  @version 1.0
 */
abstract class SymbolPairs {
  val global: Global
  import global._

  case class SymbolPair(base: Symbol, low: Symbol, high: Symbol) {
    def whichInBase   = List(low, high) filter (_.owner == base)
    def pos           = if (low.owner == base) low.pos else if (high.owner == base) high.pos else base.pos
    def self          = base.thisType
    def rootType      = base.thisType
    def lowType       = self memberType low
    def lowInfo       = self memberInfo low
    def lowErased     = erasure.specialErasure(base)(low.tpe)
    def lowClassBound = classBoundAsSeen(low.tpe.typeSymbol)

    def highType       = self memberType high
    def highInfo       = self memberInfo high
    def highErased     = erasure.specialErasure(base)(high.tpe)
    def highClassBound = classBoundAsSeen(high.tpe.typeSymbol)
    def isErroneous    = low.tpe.isErroneous || high.tpe.isErroneous
    def sameKind       = sameLength(low.typeParams, high.typeParams)

    private def classBoundAsSeen(tsym: Symbol) =
      tsym.classBound.asSeenFrom(rootType, tsym.owner)

    def sameTypeAfterErasure = exitingErasure(low.tpe =:= high.tpe)
    def memberTypesMatch     = exitingRefchecks(lowType matches highType)

    def lowAccessBoundary  = low accessBoundary base
    def highAccessBoundary = high accessBoundary base

    private def memberDefString(sym: Symbol, where: Boolean) = {
      val def_s = (
        if (sym.isConstructor) s"$sym: ${self memberType sym}"
        else sym defStringSeenAs (self memberType sym)
      )
      def_s + whereString(sym)
    }
    /** A string like ' at line 55' if the symbol is defined in the class
     *  under consideration, or ' in trait Foo' if defined elsewhere.
     */
    private def whereString(sym: Symbol) =
      if (sym.owner == base) " at line " + sym.pos.line else sym.locationString

    def lowString  = memberDefString(low, where = true)
    def highString = memberDefString(high, where = true)

    def comparisonString = s"$lowString and $highString"
    override def toString = s"""
      |Cursor(in $base) {
      |   high  $highString
      | erased  $highErased
      |  infos  ${high.infosString}
      |    low  $lowString
      | erased  $lowErased
      |  infos  ${low.infosString}
      |}""".trim.stripMargin
  }

  /** The cursor class
   *  @param base   the base class containing the participating symbols
   */
  abstract class Cursor(val base: Symbol) {
      final val self  = base.thisType   // The type relative to which symbols are seen.
    private val decls = newScope        // all the symbols which can take part in a pair.
    private val size  = bases.length

    /** A symbol for which exclude returns true will not appear as
     *  either end of a pair.
     */
    protected def exclude(sym: Symbol): Boolean

    /** Does `sym1` match `sym2` such that (sym1, sym2) should be
     *  considered as a (lo, high) pair? Types always match. Term symbols
     *  match if their member types relative to `self` match.
     */
    protected def matches(sym1: Symbol, sym2: Symbol): Boolean

    /** The parents and base classes of `base`.  Can be refined in subclasses.
     */
    protected def parents: List[Type] = base.info.parents
    protected def bases: List[Symbol] = base.info.baseClasses

    /** An implementation of BitSets as arrays (maybe consider collection.BitSet
     *  for that?) The main purpose of this is to implement
     *  intersectionContainsElement efficiently.
     */
    private type BitSet = Array[Int]

    /** A mapping from all base class indices to a bitset
     *  which indicates whether parents are subclasses.
     *
     *   i \in subParents(j)   iff
     *   exists p \in parents, b \in baseClasses:
     *     i = index(p)
     *     j = index(b)
     *     p isSubClass b
     *     p.baseType(b) == self.baseType(b)
     */
    private val subParents = new Array[BitSet](size)

    /** A map from baseclasses of <base> to ints, with smaller ints meaning lower in
     *  linearization order. Symbols that are not baseclasses map to -1.
     */
    private val index = new mutable.HashMap[Symbol, Int] { override def default(key: Symbol) = -1 }

    /** The scope entries that have already been visited as highSymbol
     *  (but may have been excluded via hasCommonParentAsSubclass.)
     *  These will not appear as lowSymbol.
     */
    private val visited = HashSet[ScopeEntry]("visited", 64)

    /** Initialization has to run now so decls is populated before
     *  the declaration of curEntry.
     */
    init()

    // The current low and high symbols; the high may be null.
    private[this] var lowSymbol: Symbol  = _
    private[this] var highSymbol: Symbol = _

    // The current entry candidates for low and high symbol.
    private[this] var curEntry  = decls.elems
    private[this] var nextEntry = curEntry

    next()

    // populate the above data structures
    private def init() {
      // Fill `decls` with lower symbols shadowing higher ones
      def fillDecls(bcs: List[Symbol], deferred: Boolean) {
        if (!bcs.isEmpty) {
          fillDecls(bcs.tail, deferred)
          var e = bcs.head.info.decls.elems
          while (e ne null) {
            if (e.sym.initialize.isDeferred == deferred && !exclude(e.sym))
              decls enter e.sym
            e = e.next
          }
        }
      }
      var i = 0
      for (bc <- bases) {
        index(bc) = i
        subParents(i) = new BitSet(size)
        i += 1
      }
      for (p <- parents) {
        val pIndex = index(p.typeSymbol)
        if (pIndex >= 0)
          for (bc <- p.baseClasses ; if matchAtBaseClass(bc)(p, self)) {
            val bcIndex = index(bc)
            if (bcIndex >= 0)
              include(subParents(bcIndex), pIndex)
          }
      }
      // first, deferred (this will need to change if we change lookup rules!)
      fillDecls(bases, deferred = true)
      // then, concrete.
      fillDecls(bases, deferred = false)
    }

    private def include(bs: BitSet, n: Int) {
      val nshifted = n >> 5
      val nmask    = 1 << (n & 31)
      bs(nshifted) |= nmask
    }

    /** Implements `bs1 * bs2 * {0..n} != 0.
     *  Used in hasCommonParentAsSubclass */
    private def intersectionContainsElementLeq(bs1: BitSet, bs2: BitSet, n: Int): Boolean = {
      val nshifted = n >> 5
      val nmask = 1 << (n & 31)
      var i = 0
      while (i < nshifted) {
        if ((bs1(i) & bs2(i)) != 0) return true
        i += 1
      }
      (bs1(nshifted) & bs2(nshifted) & (nmask | nmask - 1)) != 0
    }

    /** Do `sym1` and `sym2` have a common subclass in `parents`?
     *  In that case we do not follow their pairs.
     */
    private def hasCommonParentAsSubclass(sym1: Symbol, sym2: Symbol) = {
      val index1 = index(sym1.owner)
      (index1 >= 0) && {
        val index2 = index(sym2.owner)
        (index2 >= 0) && {
          intersectionContainsElementLeq(
            subParents(index1), subParents(index2), index1 min index2)
        }
      }
    }

    @tailrec private def advanceNextEntry() {
      if (nextEntry ne null) {
        nextEntry = decls lookupNextEntry nextEntry
        if (nextEntry ne null) {
          val high = nextEntry.sym
          if (!matches(lowSymbol, high))
            advanceNextEntry()
          else {
            visited addEntry nextEntry
            // skip nextEntry if a class in `parents` is a subclass of the
            // owners of both low and high.
            if (hasCommonParentAsSubclass(lowSymbol, high))
              advanceNextEntry()
            else
              highSymbol = high
          }
        }
      }
    }
    @tailrec private def advanceCurEntry() {
      if (curEntry ne null) {
        curEntry = curEntry.next
        if (curEntry ne null) {
          if (visited(curEntry) || exclude(curEntry.sym))
            advanceCurEntry()
          else
            nextEntry = curEntry
        }
      }
    }

    /** The `low` and `high` symbol.  In the context of overriding pairs,
     *  low == overriding and high == overridden.
     */
    def low      = lowSymbol
    def high     = highSymbol

    def hasNext    = curEntry ne null

    def sameTypeAfterPostErasure = exitingPostErasure(low.tpe =:= high.tpe)
    def currentPair = new SymbolPair(base, low, high)

    //@M: note that next is called once during object initialization (why?)
    def next() {
      if (curEntry ne null) {
        lowSymbol = curEntry.sym
        advanceNextEntry()        // sets highSymbol
        if (nextEntry eq null) {
          advanceCurEntry()
          next()
        }
      }
    }
  }
}
