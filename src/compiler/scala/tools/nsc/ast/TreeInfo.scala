/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

import reflect.internal.HasFlags
import reflect.internal.Flags._
import symtab._

/** This class ...
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
abstract class TreeInfo extends reflect.internal.TreeInfo {
  val global: Global
  import global._

  import definitions.ThrowableClass

 /** Does list of trees start with a definition of
   *  a class of module with given name (ignoring imports)
   */
  override def firstDefinesClassOrObject(trees: List[Tree], name: Name): Boolean = trees match {
    case ClassDef(_, `name`, _, _) :: Nil => true
    case _ => super.firstDefinesClassOrObject(trees, name)
  }

  def isInterface(mods: HasFlags, body: List[Tree]) =
    mods.isTrait && (body forall isInterfaceMember)

  def isAllowedInUniversalTrait(stat: Tree): Boolean = stat match {
    case _: ValDef => false
    case Import(_, _) | EmptyTree => true
    case _: DefTree => true
    case _ => false
  }
}
