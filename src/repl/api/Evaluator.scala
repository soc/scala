package scala.repl
package api

trait Evaluator {
  def parse(code: String): Tree
  def typed(code: String): Type
  def eval[T >: Null](code: String): T
}
