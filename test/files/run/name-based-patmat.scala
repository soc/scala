final class MiniSome[T](val get: T) extends AnyVal { def isEmpty = false }

package p1 {
  class Triple(val x: Any) extends AnyRef with Product3[String, String, String] {
    private def s = "" + x
    override def canEqual(x: Any) = this eq x.asInstanceOf[AnyRef]
    def isEmpty = false
    def get = this
    def _1 = s
    def _2 = "2 " + s + "s! A ha ha!"
    def _3 = "3 " + s + "s! A ha ha!"

    override def toString = s"Triple(${_1}, ${_2}, ${_3})"
  }

  object Triple {
    def unapply(x: Any): Triple = new Triple(x)
  }
}

object Test {

  // def f(x: Any) = x match {
  //   case p1.Foo(x, y, z) => println((x, y, z))
  //   case x               => println(x)
  // }

  def main(args: Array[String]): Unit = {
    "catdog" match {
      case p1.Triple(x, y, z) => List(x, y, z) foreach println
      case x                  => println("fail: " + x)
    }
  }
}
