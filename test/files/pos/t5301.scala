object Test {
  def f[A, CC[X] <: Traversable[X], Coll <: CC[A]](xs: Coll) = xs
  
  def g[B, DD[X] <: Traversable[X]](xs: DD[B]) = xs
  
  def main(args: Array[String]): Unit = {
    // f[Int, List, List[Int]](List(1))
    f(List(1))
    g(List(1))
  }
}
