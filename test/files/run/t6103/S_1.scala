trait Func1[R] {
  def apply(): R = null.asInstanceOf[R]
}

class Func1ReturnString extends Func1[String]
