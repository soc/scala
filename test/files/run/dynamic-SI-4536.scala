import language.dynamics

object Test extends App with Dynamic {
  def selectDynamic(m: String) = "DynamicClass." + m
  def applyDynamic(m: String)(a: Any*) = "DynamicClass."+m+"("+a+")"
  def regularMethod() = "Regular method"
  println(this.bar)
  println(this.bar())
  println(Test.bar())
  println(this.regularMethod)
  println(this.regularMethod())
  println(Test.regularMethod())
}

object Pos {
  object dynamicObject extends Dynamic {
    def applyDynamic(m: String)() = ()
    this.foo()
  }
  class dynamicClass extends Dynamic {
    def selectDynamic(m: String) = ()
    def applyDynamic(m: String)() = ()
    this.bar
    dynamicObject.bar()
  }
  abstract class dynamicAbstractClass extends Dynamic {
    def applyDynamic(m: String)(args: Any*): Int
    this.pili(1, new dynamicClass, "hello");
  }
  trait dynamicTrait extends Dynamic {
    def applyDynamic(m: String)(args: Any*) = 1
    def two = 2
    this.mili(1,2,3)
    two
  }
  object dynamicMixin extends dynamicAbstractClass with dynamicTrait {
    this.foo(None)
  }
}
