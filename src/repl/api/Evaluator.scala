package scala.repl
package api

import scala.reflect.api.Universe

trait Evaluator {
  val global: Universe

  import global._

  case class TypedTree(tree: Tree, tpe: Type)
  case class Eval[+T](value: T)

  def parse(code: String): Tree
  def typed(code: String): TypedTree
  def eval[T: TypeTag](code: String): Eval[T]
}
