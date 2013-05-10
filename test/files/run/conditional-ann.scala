import annotation._
import condition.log._

package cond {
  trait Symbol
  trait Type
  trait Tree
}

object Test {
  import cond._

  class CategoryLogger[T](categoryName: String) {
    @conditional[Severe with T] def severe(msg: String)   = println("[" + categoryName + "] severe: " + msg)
    @conditional[Warning with T] def warning(msg: String) = println("[" + categoryName + "] warning: " + msg)
    @conditional[Info with T] def info(msg: String)       = println("[" + categoryName + "] info: " + msg)
    @conditional[Config with T] def config(msg: String)   = println("[" + categoryName + "] config: " + msg)
    @conditional[Fine with T] def fine(msg: String)       = println("[" + categoryName + "] fine: " + msg)
  }

  val typeLogger   = new CategoryLogger[Type]("types")
  val symbolLogger = new CategoryLogger[Symbol]("symbols")
  val treeLogger   = new CategoryLogger[Tree]("trees")

  def main(args: Array[String]): Unit = {
    typeLogger.severe("oh noes")
    typeLogger.warning("kind of important")
    typeLogger.info("hello")
    typeLogger.config("relevant info")
    typeLogger.fine("extraneous info")

    symbolLogger.severe("oh noes")
    symbolLogger.warning("kind of important")
    symbolLogger.info("hello")
    symbolLogger.config("relevant info")
    symbolLogger.fine("extraneous info")

    treeLogger.severe("oh noes")
    treeLogger.warning("kind of important")
    treeLogger.info("hello")
    treeLogger.config("relevant info")
    treeLogger.fine("extraneous info")
  }
}
