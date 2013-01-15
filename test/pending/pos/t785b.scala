
object Point4 {
  class A { type T }
  case class B[X]()
  object C extends B[Int]()
  def f[X](b : B[X]) : A { type T = X } = b match {
    case C => new A { type T = Int }
  }
}

