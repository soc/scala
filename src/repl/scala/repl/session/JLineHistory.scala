/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.repl
package session

/** A straight scalification of the jline interface which mixes
 *  in the sparse jline-independent one too.
 */
trait JLineHistory extends JHistory with History {
  def size: Int
  def isEmpty: Boolean
  def index: Int
  def clear(): Unit
  def get(index: Int): CharSequence
  def add(line: CharSequence): Unit
  def replace(item: CharSequence): Unit

  def set(index: Int, item: CharSequence): Unit
  def removeFirst(): CharSequence
  def removeLast(): CharSequence
  def remove(i: Int): CharSequence

  def entries(index: Int): JListIterator[JEntry]
  def entries(): JListIterator[JEntry]
  def iterator: JIterator[JEntry]

  def current(): CharSequence
  def previous(): Boolean
  def next(): Boolean
  def moveToFirst(): Boolean
  def moveToLast(): Boolean
  def moveTo(index: Int): Boolean
  def moveToEnd(): Unit
}

object JLineHistory {
  class JLineFileHistory extends SimpleHistory with FileBackedHistory {
    // def removeRange(start: Int, count: Int): List[String] = {
    //   var lines: List[String] = Nil
    //   1 to count foreach (_ => lines ::= remove(start).toString)
    //   lines.reverse
    // }
    //
    override def add(item: CharSequence): Unit = {
      repldbg("history.add(%s)".format(item))
      if (!isEmpty && last == item)
        repldbg("Ignoring duplicate entry '" + item + "'")
      else {
        super.add(item)
        addLineToFile(item)
      }
    }
    override def toString = "History(size = " + size + ", index = " + index + ")"
  }

  def apply(): JLineHistory = try new JLineFileHistory catch { case x: Exception => new SimpleHistory() }
}
