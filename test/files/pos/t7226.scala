trait HK {
  type Rep[R]
  def unzip1[A, B, C[_]](ps: Rep[C[(A, B)]]): (Rep[C[A]], Rep[C[B]])
  def doUnzip1[T1, T2](ps: Rep[List[(T1, T2)]]) = unzip1(ps)
}
