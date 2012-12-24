class A {
  def f(x: Int) = x match {
    case 1 => 1
    case _ => x * f(x - 1)
  }
}
