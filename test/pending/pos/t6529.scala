package object foo {
  import scala.collection.generic.FilterMonadic
  implicit def imp1[A](xs: TraversableOnce[A])                 = new { def sfor(f: A => Any) = xs foreach f }
  implicit def imp2[A, Coll](xs: FilterMonadic[A, Coll])       = new { def fmfor(f: A => Any) = xs foreach f }
  implicit def imp3[A, CC[X] <: TraversableOnce[X]](xs: CC[A]) = new { def ccfor(f: A => Any) = xs foreach f }
}

package foo {
  class A {
    def f1(xs: Seq[Int]) = xs sfor println   // compiles
    def f2(xs: Int*)     = xs sfor println   // compiles
    def f3(xs: Seq[Int]) = xs fmfor println  // compiles
    def f4(xs: Int*)     = xs fmfor println  // compiles
    def f5(xs: Seq[Int]) = xs ccfor println  // compiles
    def f6(xs: Int*)     = xs ccfor println  // fails
    // ./a.scala:15: error: value ccfor is not a member of Int*
    //     def f6(xs: Int*)     = xs ccfor println  // fails
    //                               ^
    // one error found
  }
}
