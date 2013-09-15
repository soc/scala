class A(b: Bounds) {
  val Bounds(lo, hi) = b
  def f = lo + hi
}