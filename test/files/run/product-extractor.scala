object Test {
  class Bippy(x: Any) { def _1 = x.toString ; def _2 = _1.toInt ; def _3 = _2.toDouble }
  object Dingo { def unapply(x: Any) = new Bippy(x) }

  def f1 = (501: Any) match { case Dingo(a, b, c) => println(s"$a  $b  $c") }
  def f2 = (501: Any) match { case Dingo((a, b, c)) => println(s"$a  $b  $c") }

  def main(args: Array[String]): Unit = {
    f1
    f2
  }
}
