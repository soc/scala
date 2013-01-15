trait A1 {
  self1 =>

  def x1 = 5
  def x3 = 5

  def f = new AnyRef {
    self2 =>

    def x1 = 10
    def x2 = 10

    def f = new AnyRef {
      self3 =>

      def x1 = 15
      def g = self1.x1 + self2.x1 + x1 + this.x1 + /*this.x2 + */ A1.this.x3
    }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val a1 = new A1 { }
    println(a1.f.f.g)
  }
}
