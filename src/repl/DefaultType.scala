
//
// sealed class DefaultType[A, B]
// trait LowPriorityDefaultType {
//   implicit def lowPriorityDefault[A, B] = new DefaultType[A, B]
// }
// object DefaultType extends LowPriorityDefaultType {
//   implicit def defaultType[B] = new DefaultType[B, B]
//   def eval[T >: Null](code: String): T = null
// }

// trait LowPriorityExpectedType {
//   implicit object DefaultExpectedType extends ExpectedType[Any] { }
// }
// object ExpectedType extends LowPriorityExpectedType {
//   implicit def valueToExpected
// }
// class ExpectedType[T] { }
