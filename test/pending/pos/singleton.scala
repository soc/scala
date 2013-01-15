
object Test {
  val p = List(1, 2, 3)
  val q = List(1, 2)
  val r = q

  def f(xs: List[_]) = Console.println(xs match {
    case _: p.type => "p"
    case _: q.type => "q"
    case _: r.type => "r" // should be unreachable
    case _: S      => "s"
  })

  def g[T >: p.type with r.type with q.type <: Singleton](x: T) = x

  def main(args: Array[String]): Unit = {
    f(p)
    f(q)
    f(r)
    g(p) // should be callable
    g(q) // should be callable
    g(r) // should be callable
  }
}
