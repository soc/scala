object Test {
  def x1 = Array[Object]()
  def x2 = Array[Nothing]()
  def x3 = Array[Null]()
  def x4 = Array[Null with AnyRef]()
  def x5 = Array[AnyRef with Null]()
  def x6: Array[_ <: Nothing] = Array()
  def x7 = Array()
  def x8: Array[Int with Nothing] = Array()

  def f1(x: Boolean) = if (x) Array[String]("abc") else Array[Nothing]()
  def f2(x: Boolean) = if (x) Array[String]("abc") else Array[Null]()
  def f3(x: Boolean) = if (x) Array[Int](5) else Array[Nothing]()
  def f4(x: Boolean) = if (x) Array[Int](5) else Array[Null]()
  def f5(x: Boolean) = if (x) Array[Int](5, 10) else Array[Boolean](true, false)

  def g1 = List(Array[String]("abc"), Array[Nothing]())
  def g2 = List(Array[String]("abc"), Array[Null]())
  def g3 = List(Array[Int](5), Array[Nothing]())
  def g4 = List(Array[Int](5), Array[Null]())
  def g5 = List(Array[Int](5, 10), Array[Boolean](true, false))

  def show(body: => Any): Unit = {
    try println(body)
    catch { case t: Throwable => println(s"Caught: $t") }
  }

  def main(args: Array[String]): Unit = {
    println("Testing if/else lubs")
    show(f1(false).length)
    show(f1(true).length)
    show(f2(false).length)
    show(f2(true).length)
    show(f3(false).length)
    show(f3(true).length)
    show(f4(false).length)
    show(f4(true).length)
    show(f5(false).length)
    show(f5(true).length)

    println("Testing list lubs")
    show(g1.head.length)
    show(g1.last.length)
    show(g2.head.length)
    show(g2.last.length)
    show(g3.head.length)
    show(g3.last.length)
    show(g4.head.length)
    show(g4.last.length)
    show(g5.head.length)
    show(g5.last.length)

    println("Testing declared types")
    show(x1.length)
    show(x2.length)
    show(x3.length)
    show(x4.length)
    show(x5.length)
    show(x6.length)
    show(x7.length)
    show(x8.length)
  }
}
