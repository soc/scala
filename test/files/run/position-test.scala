object Test extends scala.tools.partest.PositionTest
// BEGIN POSITION TEST

object A {
  def update(x: Int, y: Long): Unit = ()
  def apply(x: Int): Long = x
}

trait B { A(0) &= 5 }
