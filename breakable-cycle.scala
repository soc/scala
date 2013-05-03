class Breakables {
  @volatile var busy: Boolean = false
  class Cycle extends Exception

  def getBusy[T](body: => T): T = synchronized {
    if (busy == true) throw new Cycle
    busy = true
    try body
    finally busy = false
  }
  class RobustLazyVal[T](body: => T) {
    private var value: Option[T] = None
    private var lastFailure: Throwable = _
    def isEmpty = value.isEmpty
    def caught: Throwable = lastFailure
    def get: Option[T] = value orElse {
      try { value = Some(body) ; value }
      catch { case x: Throwable => println("Caught " + x) ; lastFailure = x ; None }
    }
  }
  object RobustLazyVal {
    def apply[T](body: => T) = new RobustLazyVal(body)
  }
}

object Test1 extends Breakables {
  lazy val x1: Int = getBusy(x2)
  lazy val x2: Int = getBusy(x3)
  lazy val x3: Int = getBusy(5)

  def vals = List(x1, x2, x3)
  def show = vals foreach println
}

object Test2 extends Breakables {
  val x1 = RobustLazyVal[Int](getBusy(x2.get.get))
  val x2 = RobustLazyVal[Int](getBusy(x3.get.get))
  val x3 = RobustLazyVal[Int](getBusy(5))

  def vals = {
    val xs = List(x1, x2, x3)
    while (xs exists (_.isEmpty))
      xs foreach (_.get)

    xs map (_.get.get)
  }
  def show = vals foreach println
}

object Test {
  def main(args: Array[String]): Unit = {
    Test2.show
    Test1.show
  }
}
