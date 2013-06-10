package scala

final class Nat[N <: Int with Singleton] private (val n: N) {  //extends AnyVal {
  type Succ <: Int with Singleton
  type Pred <: Int with Singleton
}

object Nat {
  implicit def liftToType[T <: Int with Singleton](x: T): Nat[x.type] = new Nat[x.type](x)
}
