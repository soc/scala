object Test {
  implicit final class Ext(val s: String) extends AnyVal
  implicit final class ExtRef(val s: String)

  def main(args: Array[String]): Unit = {
    println(null.##)
    println((null: Any).##)
    println((null: Ext).##)
    println((null: ExtRef).##)
  }
}
