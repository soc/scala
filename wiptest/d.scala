// object Foo {
//   sealed abstract class <:<[-From, +To] extends (From => To) with Serializable { def apply(x: From): To = x.asInstanceOf[To] }
//   private[this] final val singleton_<:< = new <:<[Any,Any] { override def apply(x: Any): Any = x }
// }
object Foo {
  class X
  private val y = new X { def z = 1 }
}
