/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect.internal.util

object HashSet {
  def apply[T >: Null <: AnyRef](initialCapacity: Int): HashSet[T] = this("No Label", initialCapacity)
  def apply[T >: Null <: AnyRef](label: String, initialCapacity: Int): HashSet[T] =
    new HashSet[T](label, initialCapacity)
}

class HashSet[T >: Null <: AnyRef](val label: String, initialCapacity: Int) extends Set[T] with scala.collection.generic.Clearable {
  private var used = 0
  private var table = new Array[AnyRef](initialCapacity)
  private def index(x: Int): Int = math.abs(x % table.length)

  def size: Int = used
  def clear() {
    used = 0
    table = new Array[AnyRef](initialCapacity)
  }

  def removeEntry(x: T): Unit = {
    var h = index(x.##)
    var entry = table(h)
    while ((entry ne null) && x != entry) {
      h = index(h + 1)
      entry = table(h)
    }
    if (x == entry) {
      // Inserting a null might break an open addressing chain,
      // severing the link between an entry and the point from which
      // a search will begin. So we also null out everything to the
      // the right of this entry, until we reach a null, and then
      // re-add the nearly-orphaned entries.
      var rehash: List[T] = Nil
      var h1 = index(h + 1)
      while (table(h1) ne null) {
        rehash ::= table(h1).asInstanceOf[T]
        table(h1) = null
        h1 = index(h1 + 1)
      }
      rehash.reverse foreach addEntry
    }
  }

  def findEntryOrUpdate(x: T): T = {
    var h = index(x.##)
    var entry = table(h)
    while (entry ne null) {
      if (x == entry)
        return entry.asInstanceOf[T]

      h = index(h + 1)
      entry = table(h)
    }
    table(h) = x
    used += 1
    if (used > (table.length >> 2)) growTable()
    x
  }

  def findEntry(x: T): T = {
    var h = index(x.##)
    var entry = table(h)
    while ((entry ne null) && x != entry) {
      h = index(h + 1)
      entry = table(h)
    }
    entry.asInstanceOf[T]
  }

  def addEntry(x: T) {
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
  def addEntries(xs: TraversableOnce[T]) {
    xs foreach addEntry
  }

  def iterator = new Iterator[T] {
    private var i = 0
    def hasNext: Boolean = {
      while (i < table.length && (table(i) eq null)) i += 1
      i < table.length
    }
    def next(): T =
      if (hasNext) { i += 1; table(i - 1).asInstanceOf[T] }
      else null
  }

  private def addOldEntry(x: T) {
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
      if (entry ne null) addOldEntry(entry.asInstanceOf[T])
      i += 1
    }
  }
  override def toString() = "HashSet %s(%d / %d)".format(label, used, table.length)
}
