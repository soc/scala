package scala
package tools
package nsc
package ast

import scala.reflect.internal.util._
import scala.collection.mutable
import SourceTokens._

trait TreeCoverage {
  type Tree

  def childrenOf(tree: Tree): Seq[Tree]
  def positionOf(tree: Tree): Position
  def identOf(tree: Tree): Int

  case class ChunkedSource(sourceCode: String, chunks: Vector[Chunk], coverage: Map[Offset, TreeId]) {
    def codeOf(chunk: Chunk): String = sourceCode.slice(chunk.start, chunk.end).mkString
  }

  def analyze(code: String, tree: Tree): ChunkedSource = {
    val len         = code.length
    val coverage    = new Array[Int](len) // tree id of innermost tree which covers each source code offset
    val buf         = Vector.newBuilder[Chunk]
    def offsetPairs = 0 until len map (k => Offset(k) -> TreeId(coverage(k)))

    def traverse(t: Tree): Unit = {
      val pos = positionOf(t)
      val tid = identOf(t)

      pos.indices foreach (i => coverage(i) = tid)
      childrenOf(t) foreach traverse
    }
    def loop(start: Int): Unit = {
      if (start < len) {
        val spanId   = coverage(start)
        val end      = coverage.indexWhere(_ != spanId, start) match {
          case -1  => len
          case idx => idx
        }
        buf += Chunk(Offset(start), Length(end - start))
        loop(end)
      }
    }

    traverse(tree)
    loop(0)
    ChunkedSource(code, buf.result(), offsetPairs.toMap)
  }
}

//   def analyze(unit: CompilationUnit): Unit = {
//     val source  = unit.source
//     val offsets = new Array[Int](source.length)

//     def traverse(t: Tree): Unit = {
//       val pos = positionOf(t)
//       val tid = identOf(t)

//       pos.indices foreach (i => offsets(i) = tid)
//       childrenOf(t) foreach traverse
//     }

//     traverse(unit.body)
//   }
// }
