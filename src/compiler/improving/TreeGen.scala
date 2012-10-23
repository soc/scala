package improving

import scala.tools.nsc._

trait TreeGen {
  val global: Global
  import global._
  import definitions._

  def wrapWithPrintlns(onEnter: String, onExit: String)(body: Tree): Tree =
    wrapWithPrintlns(Literal(Constant(onEnter)), Literal(Constant(onExit)))(body)

  def wrapWithPrintlns(onEnter: Tree, onExit: Tree)(body: Tree): Tree = body match {
    case Block(stmts, expr) => treeCopy.Block(body, mkPrintlnCall(onEnter) :: stmts, Try(expr, Nil, mkPrintlnCall(onExit)))
    case expr               => Block(mkPrintlnCall(onEnter) :: Nil, Try(expr, Nil, mkPrintlnCall(onExit)))
  }

  def mkPrintlnCall(args: Tree*): Tree = args.toList match {
    case Nil      => Literal(Constant(()))
    case x :: Nil => gen.mkMethodCall(ConsoleModule, newTermName("println"), Nil, List(x))
    case xs       => gen.mkMethodCall(ConsoleModule, newTermName("println"), Nil, List(gen.mkTuple(xs)))
  }
}

