class A {
  def f(x: Int, y: Int, z: Int, x2: Int, y2: Int, z2: Int): Int = x2
  def f(x: Object): Int = ???

  def g(x: Int) = f(x, x, x, x, x) // so close
}
