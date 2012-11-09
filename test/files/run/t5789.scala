import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-optimise"
  def code = """
List(1).reduceLeft(_ + _)
List(1).reduceLeft(_ + _)
List(1).reduceLeft(_ + _)
List(1).reduceLeft(_ + _)
  """
}
