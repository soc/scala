import java.net.URI;

object S extends J[URI] {
  def f1(x1: String): URI = ???
  def f2(x1: URI, x2: String): URI = ???

  override def g3(x1: String): String = x1
}

object Test {
  def main(args: Array[String]): Unit = {
    val ms = Class.forName("S").getDeclaredMethods map (_.toGenericString)
    ms.sorted foreach println
  }
}
