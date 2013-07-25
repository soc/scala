import scala.collection.immutable.BitSet
import scala.collection.generic.CanBuildFrom

object Test {
  def builder[A, CC[X] <: TraversableOnce[X], Coll](xs: Coll with CC[A] with AnyRef)(implicit cbf: CanBuildFrom[_, A, CC[A]]) = cbf()

  def main(args: Array[String]) {
    println(builder(null: BitSet) += 1 result)
  }
}
