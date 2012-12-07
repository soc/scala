class A {
  def zipWith[A,B,C](a: List[A], b: List[B], f: (A,B) => C): List[C] = a zip b map f.tupled

  def f = zipWith(List(1,2), List(2,3), (a,b) => a+b)
}
