object Test extends App {
  val exs = classOf[Foo].getDeclaredMethods().map(_.getExceptionTypes).flatten.toList.sortBy(_.toString)
  println(exs)
}

class Foo {
  @throws[Exception]
  def bar1 = ???
  @throws[Throwable]("always")
  def bar2 = ???
  @throws(classOf[RuntimeException])
  def bar3 = ???
}