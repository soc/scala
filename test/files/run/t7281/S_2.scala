final class Bippy2(val x: Int) extends AnyVal

trait Trait[T <: AnyVal] { def boxed1: T = ??? } // erases to bound: BoxedAnyVal
class Bippy3 extends Trait[Bippy2] {
  override def boxed1 = new Bippy2(5) // override with unboxed
  def boxed2()        = List(new Bippy1(5), new Bippy2(5)).head // infers BoxedAnyVal

  def generic1[T <: AnyVal](x: T) = x
  def generic2[T <: Bippy2](x: T) = x
  def unboxed1(): Bippy2         = new Bippy2(5) // returns Int
  def unboxed2                   = new Bippy2(5) // infers Int
}

object Test {
  def main(args: Array[String]): Unit = {
    println("Bippy1 super: " + classOf[Bippy1].getSuperclass)
    println("Bippy2 super: " + classOf[Bippy2].getSuperclass)
    classOf[Bippy3].getMethods.filterNot(_.getDeclaringClass == classOf[Object]).map(_.toGenericString).sorted foreach println
  }
}
