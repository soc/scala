import scala.reflect.internal.Variance._

object Test {
  val variances = List(Invariant, Contravariant, Covariant, Bivariant)

  def main(args: Array[String]): Unit = {
    for (v1 <- variances ; v2 <- variances) {
      val res = v1 <:< v2

      println(f"${v1.longString}%13s <:< ${v2.longString}%-13s  ==  $res")
    }
  }
}
