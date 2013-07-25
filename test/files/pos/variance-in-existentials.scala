package foo {
  trait Base[A]
}
import foo._

package p1 {
  package one {
    trait Ord1[-A] extends Base[A]
    trait Ord2[-A] extends Base[A1 forSome { type A1 <: A }]
    trait Ord3[-A] extends Base[A1 forSome { type A1 >: A }]  // fails?

    trait Ord4[-A] { self: Base[A] => }
    trait Ord5[-A] { self: Base[A1 forSome { type A1 <: A }] => }
    trait Ord6[-A] { self: Base[A1 forSome { type A1 >: A }] => } // fails?
  }
  package two {
    trait Ord1[-A, A1 <: A] extends Base[A1]
    trait Ord2[-A, A1 >: A] extends Base[A1]   // compiles
    trait Ord3[-A, -A1 <: A] extends Base[A1]
    trait Ord4[-A, -A1 >: A] extends Base[A1]
    trait Ord5[-A, +A1 <: A] extends Base[A1]
    trait Ord6[-A, +A1 >: A] extends Base[A1]
  }
}

package p2 {
  package one {
    trait List1[+A] extends Base[A]
    trait List2[+A] extends Base[A1 forSome { type A1 <: A }]  // fails?
    trait List3[+A] extends Base[A1 forSome { type A1 >: A }]

    trait List4[+A] { self: Base[A] => }
    trait List5[+A] { self: Base[A1 forSome { type A1 <: A }] => } // fails?
    trait List6[+A] { self: Base[A1 forSome { type A1 >: A }] => }
  }
  package two {
    trait List1[+A, A1 <: A] extends Base[A1]   // compiles
    trait List2[+A, A1 >: A] extends Base[A1]
    trait List3[+A, +A1 <: A] extends Base[A1]
    trait List4[+A, +A1 >: A] extends Base[A1]
    trait List5[+A, +A1 <: A] extends Base[A1]
    trait List6[+A, +A1 >: A] extends Base[A1]
  }
}

package p3 {
  package one {
    trait Set1[A] extends Base[A]
    trait Set2[A] extends Base[A1 forSome { type A1 <: A }]
    trait Set3[A] extends Base[A1 forSome { type A1 >: A }]

    trait Set4[A] { self: Base[A] => }
    trait Set5[A] { self: Base[A1 forSome { type A1 <: A }] => }
    trait Set6[A] { self: Base[A1 forSome { type A1 >: A }] => }
  }
  package two {
    trait Set1[A, A1 <: A] extends Base[A1]
    trait Set2[A, A1 >: A] extends Base[A1]
    trait Set3[A, -A1 <: A] extends Base[A1]
    trait Set4[A, -A1 >: A] extends Base[A1]
    trait Set5[A, +A1 <: A] extends Base[A1]
    trait Set6[A, +A1 >: A] extends Base[A1]
  }
}
