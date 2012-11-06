/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package util

import TreeTracer._
import scala.collection.{ mutable, immutable }

/** A generic infrastructure for tracing recursive data structures.
 *  Specific applications include tracing the method call chain through
 *  a set of methods of interest and tracing the typing of trees.
 *  All the output can be delayed until shutdown so that one can
 *  trace sensitive structures without disturbing them, as toString
 *  still tends to force lazy data at times when one would wish otherwise.
 */
class TreeTracer { tracer =>

  type ReceiverType <: AnyRef

  implicit lazy val NodeOrdering: Ordering[TNode] = Ordering[Int] on (_.id)

  private val startTime = System.nanoTime

  def stringify(value: Any): String = {
    try "" + value
    catch { case x: Throwable => "" + x }
  }
  def rootString(root: ReceiverType): String = ""
  def receiverPos(recv: ReceiverType): String = ""
  def consolidationString: String = ""
  def isEligibleReceiver(recv: ReceiverType) = !openReceivers(recv)
  def consolidate(root: TNode): TNode = root

  def headers = "%-70s %s".format(
    "%5s/%-5s%s%25s".format("(1)", "(2)", "(3)", "(4)"),
    "(5)"
  )
  def banner = """
    |(1) Total time spent typing node (in ms)
    |(2) "Own" time (Total time - children's time)
    |(3) Monotonically increasing id assigned to nodes
    |(4) Textual content characterizing AST node
    |(5) Line in source file
    |
    |%s
    |%s
  """.stripMargin.trim.format(headers, "-" * 80)

  def quieter(s: String) = List(
    """\Qscala.collection.\E""" -> "sc."
  ).foldLeft(s) { case (s, (from, to)) => s.replaceAll(from, to) }

  def show() {
    val roots     = finished.values filter (_.pos.depth == 0) toList;
    val units     = roots map consolidate filter (_.size > 1)
    val millis    = (System.nanoTime - startTime) / 1000000L

    val stats = """
      |Recorded %d nodes (%d root) in %d ms.
      |%d nodes remain after consolidating%s.
    """.stripMargin.format(finished.size, roots.size, millis, units map (_.size) sum, consolidationString)

    println(stats + "\n" + banner)
    val pending = mutable.HashMap[Int, List[TNode]]() withDefaultValue Nil

    def closeNodes(p: Int => Boolean) {
      for ((out, finish) <- (pending filterKeys p).toList sortBy (_._1)) {
        for (outNode <- finish.reverse.sortBy(-_.pos.in)) {
          println(outNode.outString)
        }
        pending -= out
      }
    }

    for (root <- units) {
      println(rootString(root.receiver))

      for (inNode <- root.flatten) {
        closeNodes (_ <= inNode.id)
        pending(inNode.pos.out) ::= inNode
        println(quieter(inNode.inString))
      }
      // Closing the moral equivalent of a lisp ))))))))
      closeNodes(_ => true)
    }
  }

  private val finished          = mutable.HashMap[Int, TNode]()  // completed nodes
  private var currentPointer    = 0                             // index of the next new node
  private var trailingPointer   = 0                             // index of the oldest node in progress
  // private def currentDepth = childrenStack.size
  private def openReceivers = mutable.HashSet[ReceiverType]()
  private def timing[T](body: => T): (T, Long) = {
    val startTime  = System.nanoTime
    val result     = body
    val endTime    = System.nanoTime

    (result, endTime - startTime)
  }
  private def withReceiver[T](recv: ReceiverType)(body: => T): T = {
    openReceivers += recv
    try body
    finally openReceivers -= recv
  }

  private val childrenMap = mutable.Map[Int, List[Int]]() withDefaultValue Nil
  private var parentStack: List[Int] = Nil
  private def currentParent = parentStack.headOption getOrElse -1

  def currentDepth = parentStack.size
  def setParent(id: Int) = {
    parentStack ::= id
  }
  def addChild(id: Int) = {
    childrenMap(currentParent) ::= id
  }
  def advanceGeneration(): Int = {
    val id = currentPointer
    addChild(id)
    setParent(id)
    currentPointer += 1
    id
  }
  def retreatGeneration(): Int = {
    parentStack = parentStack.tail
    val out = currentPointer
    while (finished contains trailingPointer)
      trailingPointer += 1

    out
  }

  /** Enters a new node.
   */
  def enter[T](receiver: ReceiverType)(body: => T): T = {
    // if (!isEligibleReceiver(receiver))
    //   return body

    val depth             = currentDepth
    val id                = advanceGeneration()
    val ((result, nanos)) = withReceiver(receiver)(timing(body))
    val out               = retreatGeneration()
    val pos               = NodePos(id, out, depth)
    val children          = childrenMap(id) flatMap (finished get _) reverse
    val node              = new TNode(pos, nanos, receiver, children) { }

    finished(id) = node
    result
  }

  case class TNode(
    pos: NodePos,           // position
    totalNanos: Long,       // time
    receiver: ReceiverType, // tree
    children: List[TNode]   // children
  )
  extends TimingNode[TNode] {
    def id    = pos.in
    def out   = pos.out
    def depth = pos.depth

    def inIndent  = timeString + pos.indent
    def outIndent = (" " * timeString.length) + pos.indent
    def inString  = {
      val in0 = "%s%s: %s".format(inIndent, id, stringify(receiver))
      val pos0 = receiverPos(receiver)
      if (pos0 == "") in0
      else "%-70s %s".format(in0, pos0)
    }
    def outString = outIndent + """\--""" + id

    override def equals(other: Any) = other match {
      case x: TNode => id == x.id
      case _          => false
    }
    override def hashCode = pos.hashCode
    override def toString = "(%d)--(%s)".format(id, children map (_.id) mkString ",")
  }
}

object TreeTracer {
  def toMillis(nanos: Long): String = "" + (nanos / 1000000l)

  final case class NodePos(
    in: Int,      // pointer when node is entered
    out: Int,     // pointer when node is exited (always > in)
    depth: Int    // nesting/indentation depth
  ) {
    // no visible nodes between in and out
    def isSimple = width == 1
    def width    = out - in
    def indent   = "| " * depth

    override def toString = indent + in + (
      if (isSimple) "" else "/" + out
    )
    override def equals(other: Any) = other match {
      case x: NodePos => in == x.in
      case _          => false
    }
    override def hashCode = in
  }

  trait Node[N <: Node[N]] {
    self: N =>

    def children: List[N]
    def flatten: List[N] = this :: children.flatMap(_.flatten)
    def size: Int = 1 + children.map(_.size).sum
  }

  trait TimingNode[N <: TimingNode[N]] extends Node[N] {
    self: N =>

    def totalNanos: Long
    def childrenNanos: Long = children.map(_.totalNanos).sum
    def ownNanos: Long      = totalNanos - childrenNanos

    def totalMillis = totalNanos / 1000000L
    def ownMillis   = ownNanos / 1000000L
    def timeString  = {
      if (totalMillis == 0) " " * 11
      else if (totalMillis == ownMillis) "%5s/     " format totalMillis
      else "%5s/%-5s".format(totalMillis, ownMillis)
    }
  }

  /** Performs a depth-first traversal, consolidating nodes whose
   *  children are below the survival threshold.
   */
  trait ConsolidatingTree[N <: Node[N]] {
    def newChildren(node: N, children: List[N]): N
    def survives(node: N): Boolean

    def consolidate(root: N): N =
      newChildren(root, root.children mapConserve consolidate filter survives)
  }
}
