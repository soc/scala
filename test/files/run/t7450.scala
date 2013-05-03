class A {
  def f1[T](x: T) = x
  def f2[T >: Null](x: T) = x
  def f3[T <: String](x: T) = x
  def f4[T >: Null <: String](x: T) = x

  type T1
  type T2 >: Null
  type T3 <: String
  type T4 >: Null <: String
}
