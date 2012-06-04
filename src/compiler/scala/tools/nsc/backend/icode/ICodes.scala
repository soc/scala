/* NSC -- new scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */
/* NSC -- new scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package backend
package icode

import java.io.PrintWriter
import scala.collection.mutable
import scala.tools.nsc.symtab._
import analysis.{ Liveness, ReachingDefinitions }
import scala.tools.nsc.symtab.classfile.ICodeReader

/** Glue together ICode parts.
 *
 *  @author Iulian Dragos
 */
abstract class ICodes extends AnyRef
                                 with Members
                                 with BasicBlocks
                                 with Opcodes
                                 with TypeStacks
                                 with TypeKinds
                                 with ExceptionHandlers
                                 with Primitives
                                 with Printers
                                 with Repository
{
  val global: Global
  import global.{ log, debuglog, definitions, settings, perRunCaches }

  /** The ICode representation of classes */
  val classes = perRunCaches.newMap[global.Symbol, IClass]()

  /** Debugging flag */
  def shouldCheckIcode = settings.check contains global.genicode.phaseName
  def checkerDebug(msg: String) = if (shouldCheckIcode && global.opt.debug) println(msg)

  /** The ICode linearizer. */
  val linearizer: Linearizer = new ReversePostOrderLinearizer()

  def newTextPrinter() =
    new TextPrinter(new PrintWriter(Console.out, true), new DumpLinearizer)

  /** Have to be careful because dump calls around, possibly
   *  re-entering methods which initiated the dump (like foreach
   *  in BasicBlocks) which leads to the icode output olympics.
   */
  private var alreadyDumping = false

  /** Print all classes and basic blocks. Used for debugging. */

  def dumpClassesAndAbort(msg: String): Nothing = {
    if (alreadyDumping) global.abort(msg)
    else alreadyDumping = true

    Console.println(msg)
    val printer = newTextPrinter()
    classes.values foreach printer.printClass
    global.abort(msg)
  }

  def dumpMethodAndAbort(m: IMethod, msg: String): Nothing = {
    Console.println("Fatal bug in inlinerwhile traversing " + m + ": " + msg)
    m.dump()
    global.abort("" + m)
  }
  def dumpMethodAndAbort(m: IMethod, b: BasicBlock): Nothing =
    dumpMethodAndAbort(m, "found open block " + b + " " + b.flagsString)

  def checkValid(m: IMethod) {
    // always slightly dicey to iterate over mutable structures
    m foreachBlock { b =>
      if (!b.closed) {
        // Something is leaving open/empty blocks around (see SI-4840) so
        // let's not kill the deal unless it's nonempty.
        if (b.isEmpty) {
          log("!!! Found open but empty block while inlining " + m + ": removing from block list.")
          m.code removeBlock b
        }
        else dumpMethodAndAbort(m, b)
      }
    }
  }

  object liveness extends Liveness {
    val global: ICodes.this.global.type = ICodes.this.global
  }

  object reachingDefinitions extends ReachingDefinitions {
    val global: ICodes.this.global.type = ICodes.this.global
  }

  lazy val AnyRefReference: TypeKind    = REFERENCE(definitions.AnyRefClass)
  lazy val BoxedUnitReference: TypeKind = REFERENCE(definitions.BoxedUnitClass)
  lazy val NothingReference: TypeKind   = REFERENCE(definitions.NothingClass)
  lazy val NullReference: TypeKind      = REFERENCE(definitions.NullClass)
  lazy val ObjectReference: TypeKind    = REFERENCE(definitions.ObjectClass)
  lazy val StringReference: TypeKind    = REFERENCE(definitions.StringClass)
  lazy val ThrowableReference: TypeKind = REFERENCE(definitions.ThrowableClass)

  object icodeReader extends ICodeReader {
    lazy val global: ICodes.this.global.type = ICodes.this.global
  }

  /** A phase which works on icode. */
  abstract class ICodePhase(prev: Phase) extends global.GlobalPhase(prev) {
    override def erasedTypes = true
    override def apply(unit: global.CompilationUnit): Unit =
      unit.icode foreach apply

    def apply(cls: global.icodes.IClass): Unit
  }

  abstract class Linearizer {
    def linearize(c: IMethod): List[BasicBlock]
    def linearizeAt(c: IMethod, start: BasicBlock): List[BasicBlock]
  }

  /**
   * Linearize code in reverse post order. In fact, it does
   * a post order traversal, prepending visited nodes to the list.
   * This way, it is constructed already in reverse post order.
   */
  class ReversePostOrderLinearizer extends Linearizer {
    var blocks: List[BasicBlock] = Nil
    val visited = new mutable.HashSet[BasicBlock]
    val added = new mutable.BitSet

    def linearize(m: IMethod): List[BasicBlock] = {
      blocks = Nil;
      visited.clear()
      added.clear;

      m.exh foreach (b => rpo(b.startBlock));
      rpo(m.startBlock);

      // if the start block has predecessors, it won't be the first one
      // in the linearization, so we need to enforce it here
      if (m.startBlock.predecessors eq Nil)
        blocks
      else
        m.startBlock :: (blocks.filterNot(_ == m.startBlock))
    }

    def linearizeAt(m: IMethod, start: BasicBlock): List[BasicBlock] = {
      blocks = Nil
      visited.clear()
      added.clear()

      rpo(start)
      blocks
    }

    def rpo(b: BasicBlock): Unit =
      if (b.nonEmpty && !visited(b)) {
        visited += b;
        b.successors foreach rpo
        add(b)
      }

    /**
     * Prepend b to the list, if not already scheduled.
     * @return Returns true if the block was added.
     */
    def add(b: BasicBlock) = {
      debuglog("Linearizer adding block " + b.label)

      if (!added(b.label)) {
        added += b.label
        blocks = b :: blocks;
      }
    }
  }

  /** A 'dump' of the blocks in this method, which does not
   *  require any well-formedness of the basic blocks (like
   *  the last instruction being a jump).
   */
  class DumpLinearizer extends Linearizer {
    def linearize(m: IMethod): List[BasicBlock] = m.blocks
    def linearizeAt(m: IMethod, start: BasicBlock): List[BasicBlock] = sys.error("not implemented")
  }
}
