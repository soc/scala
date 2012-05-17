import scala.tools.partest.ReplTest
object Test extends ReplTest {
  def code = """
    |// should infer List[scala.collection.immutable.Seq[Nothing]]
    |// but reverted that for SI-5534.
    |val x = List(List(), Vector())
    |
    |:type Map(1 -> (1 to 10), 2 -> (1 to 10).toList)
    |
    |abstract class Foo { def x: Any } ; class Bar extends Foo { def x = Iterator(Nil) } ; class Baz extends Foo { def x = Iterable(Vector()) }
    |def f = if (true) new Bar else new Baz
  """.stripMargin
}
