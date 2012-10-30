object bar extends Foo { }

object Test {
  import bar._

  def main(args: Array[String]): Unit = {
    assert(new V(5) == new V(5))
    assert(W(5) == W(5))
  }
}
