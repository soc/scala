class A {
  def f(x: Int) = x match {
    case 1 => 1
    case _ => f(x - 1) // x * f(x - 1)
  }
}
