package foo
import scala.runtime.BoxedUnit

trait CustomFunction1[@specialized(scala.Int) T1, @specialized(scala.AnyRef) R] extends AnyRef {
  def apply(v1: T1): R
}
class UnitFunctionBridge[T] extends CustomFunction1[T, BoxedUnit] {
  override final def apply(t: T): BoxedUnit = BoxedUnit.UNIT
}
