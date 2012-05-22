/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package backend
package icode

import scala.tools.nsc.ast._
import scala.collection.{ mutable, immutable }
import mutable.ListBuffer

trait Linearizers {
  self: ICodes =>

  import global.debuglog
  import opcodes._

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
