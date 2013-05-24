package scala
package coll

trait Map[K, +V, +Repr] {
  def apply(key: K): V
  def contains(key: K): Boolean
  def keys: Foreach[K]
  def values: Foreach[V]
}

trait MutableMap[K, V, Repr] extends Map[K, V, Repr] {
  def add(key: K, value: V): this.type
  def remove(key: K): Option[V]
}

object Map {
  trait Entry[K, V] extends Product2[K, V] {
    def key: K
    def value: V
    def _1 = key
    def _2 = value
    override def canEqual(other: Any): Boolean = other.isInstanceOf[Entry[_,_]]
    override def equals(other: Any) = other match {
      case that: Entry[_,_] => key == that.key && value == that.value
      case _                => false
    }
    override def hashCode = key.## + value.##
  }
  class JavaEntry[K, V](entry: jMapEntry[K, V]) extends Entry[K, V] {
    def key   = entry.getKey
    def value = entry.getValue
  }
  class DefaultEntry[K, V](val key: K, val value: V) extends Entry[K, V]
  class KeyIsValueEntry[K](val key: K) extends Entry[K, K] { def value = key }
}

abstract class JavaMap[K, V, Repr <: jMap[K, V]] extends Map[K, V, Repr] {
  protected[this] def jmap: Repr

  def isEmpty                 = jmap.isEmpty
  def contains(key: K)        = jmap containsKey key
  def containsValue(value: V) = jmap containsValue value
  def apply(key: K)           = jmap get key

  def foreachKey(f: K => Any): Unit = keys foreach f
  def foreach(f: (K, V) => Any): Unit = entries foreach (e => f(e.getKey, e.getValue))

  def keys    = Foreach(jmap.keySet)
  def values  = Foreach(jmap.values)
  def entries = Foreach(jmap.entrySet)

  def toStringPrefix = "JavaMap"
  private def entries_s = keys map (k => s"$k -> ${apply(k)}") mkString ", "
  override def toString = s"$toStringPrefix($entries_s)"
}
