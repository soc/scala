import scala.tools.nsc.io._

object byname {
  case class ByName(pos: String, method: String, isPrivate: Boolean) {
  }

  val lines = File("data.txt").lines().toList
  val xs = lines map { line =>
    val Array(pos, method, access) = line split """ \| """
    ByName(pos, method, access == "private")
  }
  val (ps, nps) = xs partition (_.isPrivate)

  def stats(xs: Seq[ByName]) = {
    val grouped = xs groupBy (_.method)
    val stats = (grouped mapValues (_.length) toList) sortBy (-_._2)

    stats foreach { case (k, v) => println("%5d: %s".format(v, k)) }
  }

  def main(args: Array[String]): Unit = {
    println(lines.size + " by-name call sites.")
    println("private: ")
    stats(ps)
    println("\nnot private:")
    stats(nps)
  }
}
