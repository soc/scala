/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package reflect
package internal.util

// abstract class Set[+A <: AnyRef] extends scala.coll.Set[A, Set[A]] {

//   // def findEntry(x: T): T

//   // def addEntry(x: T): Unit

//   // def iterator: Iterator[T]

//   // def foreach[U](f: T => U): Unit = iterator foreach f

//   // def apply(x: T): Boolean = contains(x)

//   // def contains(x: T): Boolean =
//   //   findEntry(x) ne null

//   // def toList = iterator.toList

// }

object HashSet {
  implicit class HashSetOps[A >: Null <: AnyRef](val xs: HashSet[A]) extends AnyVal {
    import xs._

    // def findEntry(x: A): A
    // def addEntry(x: A): Unit
    // def findEntryOrUpdate(x: A): A
    def findEntryOrUpdate(x: A): A = {
      var h = index(x.##)
      var entry = table(h)
      while (entry ne null) {
        if (x == entry)
          return entry.asInstanceOf[A]

        h = index(h + 1)
        entry = table(h)
      }
      table(h) = x
      used += 1
      if (used > (table.length >> 2)) growTable()
      x
    }

    def addEntry(x: A) {
      var h = index(x.##)
      var entry = table(h)
      while (entry ne null) {
        if (x == entry) return
        h = index(h + 1)
        entry = table(h)
      }
      table(h) = x
      used += 1
      if (used > (table.length >> 2)) growTable()
    }
    def addEntries(xs: TraversableOnce[A]) {
      xs foreach addEntry
    }

  }

  def apply[A >: Null <: AnyRef](initialCapacity: Int): HashSet[A] = this("No Label", initialCapacity)
  def apply[A >: Null <: AnyRef](label: String, initialCapacity: Int): HashSet[A] =
    new HashSet[A](label, initialCapacity)
}

class HashSet[A >: Null <: AnyRef](val label: String, initialCapacity: Int) extends scala.coll.InvariantSet[A, HashSet[A]] with scala.collection.generic.Clearable {
  private var used = 0
  private var table = new Array[AnyRef](initialCapacity)
  private def index(x: Int): Int = math.abs(x % table.length)

  def isEmpty = size == 0
  def foreach(f: A => Any): Unit = iterator foreach f
  def contains(elem: A) = findEntry(elem) ne null
  def apply(elem: A) = contains(elem)

  def findEntry(x: A): A = {
    var h = index(x.##)
    var entry = table(h)
    while ((entry ne null) && x != entry) {
      h = index(h + 1)
      entry = table(h)
    }
    entry.asInstanceOf[A]
  }

  def size: Int = used
  def clear() {
    used = 0
    table = new Array[AnyRef](initialCapacity)
  }

  def iterator = new Iterator[A] {
    private var i = 0
    def hasNext: Boolean = {
      while (i < table.length && (table(i) eq null)) i += 1
      i < table.length
    }
    def next(): A =
      if (hasNext) { i += 1; table(i - 1).asInstanceOf[A] }
      else null
  }

  private def addOldEntry(x: A) {
    var h = index(x.##)
    var entry = table(h)
    while (entry ne null) {
      h = index(h + 1)
      entry = table(h)
    }
    table(h) = x
  }

  private def growTable() {
    val oldtable = table
    val growthFactor =
      if (table.length <= initialCapacity) 8
      else if (table.length <= (initialCapacity * 8)) 4
      else 2

    table = new Array[AnyRef](table.length * growthFactor)
    var i = 0
    while (i < oldtable.length) {
      val entry = oldtable(i)
      if (entry ne null) addOldEntry(entry.asInstanceOf[A])
      i += 1
    }
  }
  override def toString() = "HashSet %s(%d / %d)".format(label, used, table.length)
}
