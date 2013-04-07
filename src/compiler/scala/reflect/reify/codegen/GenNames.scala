package scala.reflect
package reify
package codegen

trait GenNames {
  self: Reifier =>

  import global._

  def reifyName(name: naming.Name) = {
    val factory = if (name.isTypeName) nme.TypeName else nme.TermName
    mirrorCall(factory, Literal(Constant(name.toString)))
  }
}
