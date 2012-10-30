trait Outer {
  trait Inner[T]
  def make[T](t: T): Inner[T] = ???
}
object Def extends Outer
