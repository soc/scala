import scala.tools.nsc.util._

object Test {
  def str(x: Int, y: Int): String = "" + ((x, y))
  def show(x: Int, y: Int): Unit  = println(str(x, y))

  def main(args: Array[String]): Unit = {
    PairwiseIterator sliding (1 to 5) foreach show
    PairwiseIterator(1 to 5)(0 until _ % 3) foreach show
    println(PairwiseIterator(1 to 5)(x => x :: Nil) map (_ + _) mkString " ")
  }
}
