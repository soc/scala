import scala.runtime.BoxedUnit
import scala.runtime.AbstractFunction1

// object japi {
//   @deprecated("Do not use this directly, use subclasses of this", "2.0")
//   class UnitFunctionBridge[-T] extends AbstractFunction1[T, BoxedUnit] {
//     override final def apply(t: T): BoxedUnit = {
//       internal(t)
//       BoxedUnit.UNIT
//     }
//     protected def internal(result: T): Unit = ()
//   }
// }

// abstract class Foreach[-T] extends japi.UnitFunctionBridge[T] {
//   override final def internal(t: T): Unit = each(t)
//   /**
//    * This method will be invoked once when/if a Future that this callback is registered on
//    * becomes successfully completed
//    */
//   @throws(classOf[Throwable])
//   def each(result: T): Unit
// }

// class Future[T] { def foreach[U](f: T => U): U = sys.error("foo") }

object japi {
@deprecated("Do not use this directly, use subclasses of this", "2.0")
  class UnitFunctionBridge[-T] extends (T ⇒ BoxedUnit) {
    override final def apply(t: T): BoxedUnit = {
      internal(t)
      BoxedUnit.UNIT
    }
    protected def internal(result: T): Unit = ()
  }
}

abstract class Foreach[-T] extends japi.UnitFunctionBridge[T] {
  override final def internal(t: T): Unit = each(t)

  /**
   * This method will be invoked once when/if a Future that this callback is registered on
   * becomes successfully completed
   */
  @throws(classOf[Throwable])
  def each(result: T): Unit
}
