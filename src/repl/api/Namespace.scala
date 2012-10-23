package scala.repl
package api

trait Namespace {
  type Symbol

  def types(id: String): Symbol
  def terms(id: String): Symbol
  def symbols(id: String): Symbol
}
