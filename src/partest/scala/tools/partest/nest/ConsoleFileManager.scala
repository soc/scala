/* NEST (New Scala Test)
 * Copyright 2007-2012 LAMP/EPFL
 * @author Philipp Haller
 */

package scala.tools.partest
package nest

class ConsoleFileManager(val buildPrefix: String) extends FileManager {
  def this() = this("build/pack")
}
