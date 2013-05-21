// object Test {
//   class Inv[T]

//   def foo[S](interface: Inv[_ >: S], implementation: Inv[S]) {}

//   def bar[R, T <: R](interface: Inv[R], impl: Inv[T]) {
//     foo[T](interface, impl)
//     // foo(interface, impl) // Compilation Error
//     // Inv[R] <: Inv[_ >: S]
//     // Inv[T] <: Inv[S]
//     // ----------------------
//     // R >: S
//     // T == S
//   }
// }

// object Test2 {
//   def f[T](x1: Set[T]) = () => new { def apply(x2: Set[_ <: T]) = List(x1, x2) }
// }

object Test3 {
  trait H[F[_]]
  def f[F[_], T, FT <: F[T]](h : H[F]) = 1
  f(new H[Set]{})
}
