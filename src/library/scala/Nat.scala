package scala

sealed trait Nat[N <: Int with Singleton] {
  type Succ <: Int with Singleton
  type Pred <: Int with Singleton
}

object Nat {
  implicit def liftToType[T <: Int with Singleton](x: T): Nat[x.type] = null
}