trait A[This <: A[This]] {
  def copy(): This
  def a(): This = copy()
}

trait B[This <: B[This]] extends A[This] {
  def b(): This = copy()
}

class C extends A[C] with B[C] {
  def copy() = this
  override def toString = "class C"
}
