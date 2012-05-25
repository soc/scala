/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Gilles Dubochet
 */

package scala.reflect.makro
package runtime

trait Reifiers {
  self: Context =>

  import mirror._
  import definitions._

  lazy val reflectMirrorPrefix: Tree = ReflectMirrorPrefix

  def reifyTree(prefix: Tree, tree: Tree): Tree = {
    val result = scala.reflect.reify.`package`.reifyTree(mirror)(callsiteTyper, prefix, tree)
    logFreeVars(enclosingPosition, result)
    result
  }

  def reifyType(prefix: Tree, tpe: Type, dontSpliceAtTopLevel: Boolean = false, concrete: Boolean = false): Tree = {
    val result = scala.reflect.reify.`package`.reifyType(mirror)(callsiteTyper, prefix, tpe, dontSpliceAtTopLevel, concrete)
    logFreeVars(enclosingPosition, result)
    result
  }

  def reifyClass(tpe: Type): Tree =
    scala.reflect.reify.`package`.reifyClass(mirror)(callsiteTyper, tpe)

  def unreifyTree(tree: Tree): Tree =
    Select(tree, definitions.ExprEval)
}
