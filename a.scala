class Foo {
  def iterator: Iterator[String] = new Iterator[String] {
    var index = 0
    val initialModCount = 1
    def hasNext = false
    def next = ???
  }
}
