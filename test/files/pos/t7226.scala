trait HK {
  type Rep[A]
  def unzip1[A, B, C[_]](ps: Rep[C[(A, B)]]): (Rep[C[A]], Rep[C[B]])
  def doUnzip1[A, B](ps: Rep[List[(A, B)]]) = unzip1(ps)
}
