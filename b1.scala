trait Boo extends PF[Nothing, Any] {
  def dingo = 5
}

trait BooHoo extends Boo {
  override def dingo = 10
}


abstract class Bippy[-T1, +R] extends PF[T1, R] {
  def isDefinedAt(x: T1): Boolean = true
}

object Bop extends Bippy[Int, String] with BooHoo {
  override def isDefinedAt(x: Int) = false
}
