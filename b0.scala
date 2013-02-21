trait PF[-A, +B] {
  def isDefinedAt(x: A): Boolean
  def orElse[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]): PartialFunction[A1, B1] = ???
}
