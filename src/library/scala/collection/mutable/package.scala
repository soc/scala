package scala
package collection
package object mutable {
  val HashMap = CompactHashMap
  type HashMap[A, B] = CompactHashMap[A, B]
}
