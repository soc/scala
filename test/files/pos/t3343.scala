import scala.collection.mutable.{ Builder, ListBuffer }

object Test {
  class Converter[T]
  object SimpleIntConverter extends Converter[Int]

  class TraversableConverter[T, CC[X] <: Traversable[X]](converter: Converter[T], builder: Builder[T, CC[T]]) extends Converter[CC[T]] {
    def convert(x: T): List[T] = List(x)
  }
  val tc: Converter[List[Int]] = new TraversableConverter(SimpleIntConverter, new ListBuffer[Int])
  val tc2 = new TraversableConverter(SimpleIntConverter, new ListBuffer[Int])

  def main(args: Array[String]): Unit = {
  }
}
