package scala
package coll

trait InvariantSet[A, +Repr] extends InvariantContainer[A, Repr] {
  def contains(elem: A): Boolean
}

trait Set[+A, +Repr] extends CovariantContainer[A, Repr] {
  def unsafeContains[A1 >: A](elem: A1): Boolean
}

abstract class JavaSet[A, Repr <: jSet[A]] extends InvariantSet[A, Repr] {
  protected[this] def jset: Repr
  def size = jset.size
  def isEmpty = jset.isEmpty
  def contains(elem: A) = jset contains elem
  def apply(elem: A): Boolean = contains(elem)
  def foreach(f: A => Any): Unit = {
    val it = jset.iterator
    while (it.hasNext) f(it.next)
  }
  def toStringPrefix = "JavaSet"
  override def toString = jset.asScala.mkString(toStringPrefix + "(", ", ", ")")
}

abstract class JavaMapBackedSet[A, Repr <: jMap[A, A]] extends InvariantSet[A, Repr] with Clearable {
  protected[this] def jmap: Repr

  def clear(): Unit = jmap.clear()

  def get(elem: A): A              = jmap get elem
  def getOrAdd(elem: A): A         = if (contains(elem)) get(elem) else add(elem)
  def add(elem: A): A              = { jmap.put(elem, elem) ; elem }
  def addAll(elems: A*): this.type = { elems foreach add ; this }

  def size              = jmap.size
  def isEmpty           = jmap.isEmpty
  def contains(elem: A) = jmap containsKey elem
  def apply(elem: A)    = jmap containsKey elem

  def foreach(f: A => Any): Unit = Foreach(jmap.keySet) foreach f
  def toStringPrefix = "JavaMapBackedSet"
}

class JavaHashMapBackedSet[A](protected[this] val jmap: jHashMap[A, A]) extends JavaMapBackedSet[A, jHashMap[A, A]] {
  override def toStringPrefix = "JavaHashMapBackedSet"
}
object JavaHashMapBackedSet {
  def apply[A](initialCapacity: Int = 16)(xs: A*): JavaHashMapBackedSet[A] = {
    val map = new JavaHashMapBackedSet[A](new jHashMap[A, A](initialCapacity))
    xs foreach map.add
    map
  }
}

class MutableJavaSet[A, Repr <: jSet[A]](protected[this] val jset: Repr) extends JavaSet[A, Repr] with Clearable {
  override def toStringPrefix = "MutableJavaSet"

  def clear(): Unit                        = jset.clear()
  def add(elem: A): this.type              = { jset add elem ; this }
  def addAll(elems: A*): this.type         = { elems foreach add ; this }
  def addAll(elems: Foreach[A]): this.type = { elems foreach add ; this }
}

class MutableJavaHashSet[A](initialCapacity: Int, loadFactor: Float)
      extends MutableJavaSet[A, jHashSet[A]](new jHashSet[A](initialCapacity, loadFactor))

class MutableJavaTreeSet[A: Ordering]()
      extends MutableJavaSet[A, jTreeSet[A]](new jTreeSet[A](implicitly[Ordering[A]].asInstanceOf[Comparator[A]]))

object MutableJavaTreeSet {
  def compare[A](lt: (A, A) => Boolean): MutableJavaTreeSet[A] = apply[A]()(Ordering fromLessThan lt)
  def apply[A: Ordering](xs: A*): MutableJavaTreeSet[A]        = new MutableJavaTreeSet[A]().addAll(xs: _*)
}

object MutableJavaHashSet {
  def apply[A](initialCapacity: Int = 16, loadFactor: Float = 0.75f)(xs: A*): MutableJavaHashSet[A] =
    new MutableJavaHashSet[A](initialCapacity, loadFactor).addAll(xs: _*)
}

object MutableJavaSet {
  def apply[A, Repr <: jSet[A]](jset: Repr) = new MutableJavaSet[A, Repr](jset)
}

class ImmutableJavaSet[A, Repr <: jSet[A]](protected[this] val jset: Repr) extends JavaSet[A, Repr] {
  override def toStringPrefix = "ImmutableJavaSet"
}
object ImmutableJavaSet {
  def treeSet[A: Ordering](xs: A*): ImmutableJavaSet[A, jTreeSet[A]] = {
    val jset = new jTreeSet[A](implicitly[Ordering[A]].asInstanceOf[java.util.Comparator[A]])
    xs foreach (jset add _)
    new ImmutableJavaSet[A, jTreeSet[A]](jset)
  }

  def hashSet[A](xs: A*): ImmutableJavaSet[A, jHashSet[A]] = {
    val jset = new jHashSet[A]
    xs foreach (jset add _)
    new ImmutableJavaSet[A, jHashSet[A]](jset)
  }
}

// object JTreeSet {
//   def apply
// }


// class JTreeSet[+A](private[this] val jset: java.util.TreeSet[A]) extends Set[A, JTreeSet[A]] {
//   def unsafeContains[A1 >: A](elem: A1) = jset contains elem
//   def isEmpty = jset.isEmpty
//   def foreach(f: A => Any): Unit = {
//     val it = jset.iterator
//     while (it.hasNext) f(it.next)
//   }

//   override def toString = jset.asScala.mkString("Set(", ", ", ")")
// }
