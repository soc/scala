import scala.tools.partest._

object Test extends DirectTest {
  override def extraSettings: String = "-usejavacp -Xprint:patmat -Xprint-pos -Yrangepos -d " + testOutput.path
  override def code = """
package s

sealed abstract protected[s] class A {
  protected implicit def f: Int
  lazy val g = Nil ++ Nil ++ Nil
}""".trim

  override def show() { Console.withErr(System.out)(compile()) }
}
