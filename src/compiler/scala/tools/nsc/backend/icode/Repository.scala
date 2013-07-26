/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package icode

import scala.collection.{ mutable, immutable }
import java.io.IOException

/**
 *  @author Iulian Dragos
 */
trait Repository {
  val global: Global
  import global._
  import icodes._

  val loaded: mutable.Map[Symbol, IClass] = perRunCaches.newMap()

  /** Is the given class available as icode? */
  def available(sym: Symbol) = classes.contains(sym) || loaded.contains(sym)

  /** The icode of the given class, if available */
  def icode(sym: Symbol): Option[IClass] = (classes get sym) orElse (loaded get sym)

  /** Load bytecode for given symbol. */
  def load(sym: Symbol): Boolean = {
    try {
      val (c1, c2) = icodeReader.readClass(sym)
      assert(c1.symbol == sym || c2.symbol == sym, s"c1.symbol=${c1.symbol}, c2.symbol=${c2.symbol}, sym=$sym")
      loaded(c1.symbol) = c1
      loaded(c2.symbol) = c2
      true
    }
    catch {
      case e @ (_: MissingRequirementError | _: IOException | _: TypeError) =>
        devWarning(s"Failed to load ${sym.fullName}. [${e.getMessage}]")
        if (settings.debug) { e.printStackTrace }
        false
      case t: Throwable =>
        devWarning(s"Here's a nice exception I like to catch! $t")
        t.printStackTrace
        false
    }
  }
}
