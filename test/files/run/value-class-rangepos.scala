import scala.util.Random.nextInt
import scala.math.abs
import scala.reflect.RangePos

object Test {
  val xs = List(-1, 0, 1, RangePos.MaxValue / 2, RangePos.MaxValue * 2 / 3, RangePos.MaxValue, RangePos.MaxValue + 1)

  def main(args: Array[String]): Unit = {
    for (start <- xs ; point <- xs ; end <- xs) {
      print("(%7s %7s %7s)  ".format(start, point, end))
      try {
        val p = RangePos(start, point, end)
        println("%25s   %s".format(p, p.makeTransparent))
      }
      catch { case ex: IllegalArgumentException => println(ex.getMessage) }
    }
  }
}
