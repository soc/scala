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
