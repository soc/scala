package bar {
  class M[+A](a: A)
  object MInt extends M[Int](5) { }
  object ExMInt { def unapply(m: M[Int]) = true }

  object Test {
    def f0 = MInt match { case _: M[String] => () }
    def f1 = (new M[String]("")) match { case _: MInt.type => () }
    def f2 = (new M[String]("")) match { case ExMInt() => () }

    def g0 = (MInt: M[Any]) match { case _: M[String] => () }
    def g1 = ((new M[String]("")): M[Any]) match { case _: MInt.type => () }
    def g2 = ((new M[String]("")): M[Any]) match { case ExMInt() => () }
  }
}

// package baz {
//   class M[A](a: A)
//   object MInt extends M[Int](5) { }
//   object ExMInt { def unapply(m: M[Int]) = true }

//   object Test {
//     def f = (new M[String]("")) match { case _: MInt.type => 1 ; case _ => 2 }
//     def g = (new M[String]("")) match { case ExMInt() => 1 ; case _ => 2 }
//     def h = (new M[Any]("")) match { case ExMInt() => 1 ; case _ => 2 }
//   }
// }

// package quux {
//   trait Foo[A]
//   trait Bar[A]
//   class Bip[A]

//   trait M[A]
//   object MInt extends M[Int] { }
//   object ExMInt { def unapply(m: M[Int]) = true }

//   object Test {
//     def f[A](x: M[A]) = x match { case _: MInt.type => 1 ; case _ => 2 }  // ok
//     def g(xs: List[Int]) = xs match { case _: List[String] => 1 ; case _ => 2 } // fail
//     def h(xs: List[String]) = xs match { case _: List[AnyRef] => 1 ; case _ => 2 } // ok

//     def fn1(x: Foo[Int]) = x match { case _: Bar[_] => 1 ; case _ => 2 } // ok
//     def fn2(x: Bar[Int]) = x match { case _: Foo[_] => 1 ; case _ => 2 } // ok
//     def fn3(x: Bip[Int]) = x match { case _: Foo[_] => 1 ; case _ => 2 } // ok
//     def fn4(x: Bip[Int]) = x match { case _: Bar[_] => 1 ; case _ => 2 } // ok
//     def fn5(x: Foo[Int] with Bar[Int]) = x match { case _: Bip[_] => 1 ; case _ => 2 } // ok
//   }
// }
