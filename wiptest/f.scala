abstract class A[T] {
  val foo: Set[_ <: T] = null
  val bar: Set[_ <: T]
}