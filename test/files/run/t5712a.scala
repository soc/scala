class A {
  def f[T1, T2](p1: T1, p2: T2)(q1: T1 = p1) = q1

  def g1                 = f(1, 2)()
  def g2                 = f(1, 2)(2)
  def g3                 = f(1, 2)(_)
  def g4[T](p: T)        = f(p, 2)()
  def g5[T >: Int](p: T) = f(p, 2)(5)
  def g6[T]              = f(1, 2)(_)
  def g7[T](p: T)        = f(p, 2)(p)
}

object Test extends A {
  def main(args: Array[String]): Unit = {
    List(g1, g2, g3(3), g4(4), g5(5), g6(6), g7(7)) foreach println
  }
}
