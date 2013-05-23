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

class MutableJavaSet[A, Repr <: jSet[A]](protected[this] val jset: Repr) extends JavaSet[A, Repr] {
  override def toStringPrefix = "MutableJavaSet"

  def addEntry(elem: A): this.type = { jset add elem ; this }
}

object MutableJavaSet {
  def apply[A](lessThan: (A, A) => Boolean) = treeSet()(Ordering fromLessThan lessThan)

  def treeSet[A: Ordering](xs: A*): MutableJavaSet[A, java.util.TreeSet[A]] = {
    val jset = new java.util.TreeSet[A](implicitly[Ordering[A]].asInstanceOf[java.util.Comparator[A]])
    xs foreach (jset add _)
    new MutableJavaSet[A, java.util.TreeSet[A]](jset)
  }

  def hashSet[A](xs: A*): MutableJavaSet[A, java.util.HashSet[A]] = {
    val jset = new java.util.HashSet[A]
    xs foreach (jset add _)
    new MutableJavaSet[A, java.util.HashSet[A]](jset)
  }
}


class ImmutableJavaSet[A, Repr <: jSet[A]](protected[this] val jset: Repr) extends JavaSet[A, Repr] {
  override def toStringPrefix = "ImmutableJavaSet"
}
object ImmutableJavaSet {
  def treeSet[A: Ordering](xs: A*): ImmutableJavaSet[A, java.util.TreeSet[A]] = {
    val jset = new java.util.TreeSet[A](implicitly[Ordering[A]].asInstanceOf[java.util.Comparator[A]])
    xs foreach (jset add _)
    new ImmutableJavaSet[A, java.util.TreeSet[A]](jset)
  }

  def hashSet[A](xs: A*): ImmutableJavaSet[A, java.util.HashSet[A]] = {
    val jset = new java.util.HashSet[A]
    xs foreach (jset add _)
    new ImmutableJavaSet[A, java.util.HashSet[A]](jset)
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
