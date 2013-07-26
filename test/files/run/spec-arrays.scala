class Generic[T](a: Array[T]) {
  def apply() = a(0)
}

class Spec[@specialized(AnyRef) T](a: Array[T]) {
  def apply() = a(0)
}

object Test {
  val runtime = new scala.runtime.InstrumentedScalaRunTime
  scala.runtime.ScalaRunTime.setRunTime(runtime)

  def main(args: Array[String]) {
    val len = 50

    testSpec(new Array[String](len))
    runtime.printArrayCounts()

    (new Spec(new Array[String](len)))()
    runtime.printArrayCounts()

    testGeneric(new Array[String](len))
    runtime.printArrayCounts()

    (new Generic(new Array[String](len)))()
    runtime.printArrayCounts()
  }

  def testGeneric[T](a: Array[T]) = {
    var i = 0
    var sum = 0
    while (i < a.length) {
      sum += (if (a(i) != null) 1 else 0)
      i += 1
    }
    sum
  }

  def testSpec[@specialized(AnyRef) T](a: Array[T]) = {
    var i = 0
    var sum = 0
    while (i < a.length) {
      sum += (if (a(i) != null) 1 else 0)
      i += 1
    }
    sum
  }

}
