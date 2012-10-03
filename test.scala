import improving._
import improving.MacroStringAdd._

class A {
  val any2stringadd = null
  val x             = List(123)

  def f: String = x +! "b" +! (List(1,2,3) map (_ + 1)) +! "c"
}
