package object foo {
  def newReplValue[T <: AnyRef](x: T): ReplValue[x.type, T] = new ReplValue[x.type, T](x)

  implicit def recoverReplValue[T <: AnyRef](x: T): RecoverReplValue[x.type] = new RecoverReplValue[x.type](x)
}

package foo {
  class RecoverReplValue[T <: AnyRef](x: T) {
    def currentValue[U](implicit rt: ReplValue[x.type, U]): U = rt.value
  }
  class ReplValue[T <: AnyRef, U](val value: U) { }

  object Test {
    def main(args: Array[String]): Unit = {
      val x = List(1, 2, 3)
      implicit val impx = newReplValue(x)
      val y = List(4, 5, 6)
      implicit val impy = newReplValue(y)

      println(x.currentValue)
      println(y.currentValue)
    }
  }
}