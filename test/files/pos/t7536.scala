trait Base[A]

trait Ord1[-A] extends Base[A1 forSome { type A1 >: A }]  // fails
trait Ord2[-A, A1 >: A] extends Base[A1]                  // compiles

trait Ord3[+A] extends Base[A1 forSome { type A1 <: A }]  // fails
trait Ord4[+A, A1 <: A] extends Base[A1]                  // compiles
