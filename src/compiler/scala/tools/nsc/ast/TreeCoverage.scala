package scala
package tools
package nsc
package ast

// import scala.reflect.internal.util._
// import scala.collection.mutable
// import SourceTokens._

trait TreeCoverage { }
//   type Tree <: AnyRef

//   def EmptyTree: Tree
//   def childrenOf(tree: Tree): Seq[Tree]
//   def positionOf(tree: Tree): Position
//   def identOf(tree: Tree): Int
//   def isIndentingTree(tree: Tree): Boolean

//   // def ownOffsets(tree: Tree): Vector[Offset] = {
//   //   val exclude = (tree.children map positionOf filter (_.isOpaqueRange) flatMap (_.indices)).toSet
//   //   (positionOf(tree).indices filterNot exclude).toVector
//   // }

//   // final case class Chunk(tree: Tree, depth: Depth, offset: Offset, length: Length) {
//   // final case class Cover(tree: Tree, offsets: Vector[Offset]) {
//   //   def treePos = positionOf(tree)
//   //   def start   = offset.value
//   //   def end     = start + length.value
//   //   def indices = start until end
//   // }

//   // case class ChunkedSource(sourceCode: String, chunks: Vector[Chunk]) {
//   //   def codeOf(chunk: Chunk): String = sourceCode.slice(chunk.start, chunk.end).mkString

//   //   override def toString = chunks map (c => "%-50s // %s".format(codeOf(c), shortClassOfInstance(coverage(c.offset)))) mkString "\n"
//   // }

//   def reverseTreeMap(tree: Tree): Map[Tree, Tree] = {
//     val parents = mutable.Map[Tree, Tree]()
//     def loop(t: Tree): Unit = childrenOf(t) foreach { c =>
//       parents(c) = t
//       loop(c)
//     }
//     loop(tree)
//     parents.toMap
//   }

//   def analyze(code: String, tree: Tree): Vector[Chunk] = {
//     val parentMap = reverseTreeMap(tree)
//     val len       = code.length
//     val coverage  = new Array[Int](len) // tree id of innermost tree which covers each source code offset
//     val buf       = Vector.newBuilder[Chunk]
//     val trees     = mutable.Map[Int, Tree]() withDefaultValue EmptyTree
//     // val depths    = mutable.Map[Int, Depth]() withDefaultValue Depth(0)

//     def offsetMap = Map(0 until len map (k => Offset(k) -> trees(coverage(k))): _*)

//     // def parentChain(t: Tree): List[Tree] = parentMap get t match {
//     //   case Some(p) => p :: parentChain(p)
//     //   case _       => Nil
//     // }
//     // def parentDepth(t: Tree) = Depth((parentChain(t) filter isIndentingTree).length)

//     def traverse(t: Tree): Unit = {
//       val tid = identOf(t)
//       val pos = positionOf(t)

//       trees(tid) = t
//       // depths(tid) = depth

//       if (pos.isOpaqueRange) {
//         pos.indices foreach (i => coverage(i) = tid)
//         childrenOf(t) foreach traverse
//         // (c => traverse(c, Depth(depth.value + 1)))
//       }
//     }
//     def loop(start: Int): Unit = {
//       if (start < len) {
//         val spanId   = coverage(start)
//         val node     = trees(spanId)
//         val end      = coverage.indexWhere(_ != spanId, start) match {
//           case -1  => len
//           case idx => idx
//         }
//         val own = (start until end).toVector collect { case idx if coverage(idx) == spanId => Offset(idx) }
//         buf += Chunk(node, Offset(start), Length(end - start), own)

//         var nextStart = end
//         while (nextStart < len && code.charAt(nextStart).isWhitespace) nextStart += 1
//         loop(nextStart)
//       }
//     }

//     traverse(tree)
//     loop(0)
//     buf.result()
//   }
// }
