/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.repl
package session

/** An implementation-agnostic history interface which makes no
 *  reference to the jline classes.  Very sparse right now.
 */
trait History {
  def add(item: CharSequence): Unit
  def asStrings: List[String]
  def grep(s: String): List[String]
  def index: Int
  def remove(index: Int): CharSequence
  def removeRange(start: Int, count: Int): List[String]
  def set(index: Int, item: CharSequence): Unit
  def size: Int
}
object NoHistory extends History {
  def add(item: CharSequence)             = ()
  def asStrings                           = Nil
  def grep(s: String)                     = Nil
  def index                               = 0
  def remove(index: Int)                  = null
  def removeRange(start: Int, count: Int) = Nil
  def set(index: Int, item: CharSequence) = ()
  def size                                = 0
}

object History {
  def empty: History = NoHistory
}
