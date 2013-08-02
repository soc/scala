package s {
  sealed trait C[+A]

  case class C00[+A]() extends C[A]
  case class C10[+A](x: A) extends C[A]
  case class C20[+A](x: A, y: A) extends C[A]
  case class C01[+A](xs: A*) extends C[A]
  case class C11[+A](x: A, ys: A*) extends C[A]
  case class C21[+A](x: A, y: A, zs: A*) extends C[A]

  object E00 { def unapply[A](x: Any): Boolean                   = ??? }
  object E10 { def unapply[A](x: Any): Option[A]                 = ??? }
  object E20 { def unapply[A](x: Any): Option[(A, A)]            = ??? }
  object E01 { def unapplySeq[A](x: Any): Option[Seq[A]]         = ??? }
  object E11 { def unapplySeq[A](x: Any): Option[(A, Seq[A])]    = ??? }
  object E21 { def unapplySeq[A](x: Any): Option[(A, A, Seq[A])] = ??? }

  object F00 { def unapply[A](x: C[A]): Boolean                   = ??? }
  object F10 { def unapply[A](x: C[A]): Option[A]                 = ??? }
  object F20 { def unapply[A](x: C[A]): Option[(A, A)]            = ??? }
  object F01 { def unapplySeq[A](x: C[A]): Option[Seq[A]]         = ??? }
  object F11 { def unapplySeq[A](x: C[A]): Option[(A, Seq[A])]    = ??? }
  object F21 { def unapplySeq[A](x: C[A]): Option[(A, A, Seq[A])] = ??? }
}
import s._

package pos {
  object Test {
    def f00(x: Any) = x match { case C00()              => 1 ; case E00() => 2 }
    def f10(x: Any) = x match { case C10(x)             => 1 ; case E10(x) => 2 }
    def f20(x: Any) = x match { case C20(x, y)          => 1 ; case E20(x, y) => 2 }
    def f01(x: Any) = x match { case C01(xs @ _*)       => 1 ; case E01(xs @ _*) => 2 }
    def f11(x: Any) = x match { case C11(x, ys @ _*)    => 1 ; case E11(x, ys @ _*) => 2 }
    def f21(x: Any) = x match { case C21(x, y, zs @ _*) => 1 ; case E21(x, y, zs @ _*) => 2 }

    def g00[A](x: C[A]) = x match { case C00()              => 1 ; case E00() => 2 }
    def g10[A](x: C[A]) = x match { case C10(x)             => 1 ; case E10(x) => 2 }
    def g20[A](x: C[A]) = x match { case C20(x, y)          => 1 ; case E20(x, y) => 2 }
    def g01[A](x: C[A]) = x match { case C01(xs @ _*)       => 1 ; case E01(xs @ _*) => 2 }
    def g11[A](x: C[A]) = x match { case C11(x, ys @ _*)    => 1 ; case E11(x, ys @ _*) => 2 }
    def g21[A](x: C[A]) = x match { case C21(x, y, zs @ _*) => 1 ; case E21(x, y, zs @ _*) => 2 }
  }
}

package neg {
  object Fail {
    def ga1[A](x: Any) = x match { case C00() => 1 ; case C10(x) => 2 ; case C20(x, y) => 3 ; case C01(xs) => 4 ; case C11(x, ys) => 5 ; case C21(x, y, zs) => 6 }
    def ga2[A](x: Any) = x match { case C00() => 1 ; case C10(x) => 2 ; case C20(x, y) => 3 ; case C01(xs) => 4 ; case C11(x, ys) => 5 ; case C21(x, y, zs) => 6 }
    def ga3[A](x: Any) = x match { case C00() => 1 ; case C10(x) => 2 ; case C20(x, y) => 3 ; case C01(xs) => 4 ; case C11(x, ys) => 5 ; case C21(x, y, zs) => 6 }
    def ga4[A](x: Any) = x match { case C00() => 1 ; case C10(x) => 2 ; case C20(x, y) => 3 ; case C01(xs) => 4 ; case C11(x, ys) => 5 ; case C21(x, y, zs) => 6 }
    def ga5[A](x: Any) = x match { case C00() => 1 ; case C10(x) => 2 ; case C20(x, y) => 3 ; case C01(xs) => 4 ; case C11(x, ys) => 5 ; case C21(x, y, zs) => 6 }
    def ga6[A](x: Any) = x match { case C00() => 1 ; case C10(x) => 2 ; case C20(x, y) => 3 ; case C01(xs) => 4 ; case C11(x, ys) => 5 ; case C21(x, y, zs) => 6 }

    def gb1[A](x: C[A]) = x match { case E00() => ??? ; case E10(x) => x ; case E20(x, y) => x ; case E01(xs @ _*) => xs.head ; case E11(x, ys) => x ; case E21(x, y, zs) => x }
    def gb2[A](x: C[A]) = x match { case E00() => ??? ; case E10(x) => x ; case E20(x, y) => x ; case E01(xs @ _*) => xs.head ; case E11(x, ys) => x ; case E21(x, y, zs) => x }
    def gb3[A](x: C[A]) = x match { case E00() => ??? ; case E10(x) => x ; case E20(x, y) => x ; case E01(xs @ _*) => xs.head ; case E11(x, ys) => x ; case E21(x, y, zs) => x }
    def gb4[A](x: C[A]) = x match { case E00() => ??? ; case E10(x) => x ; case E20(x, y) => x ; case E01(xs @ _*) => xs.head ; case E11(x, ys) => x ; case E21(x, y, zs) => x }
    def gb5[A](x: C[A]) = x match { case E00() => ??? ; case E10(x) => x ; case E20(x, y) => x ; case E01(xs @ _*) => xs.head ; case E11(x, ys) => x ; case E21(x, y, zs) => x }
    def gb6[A](x: C[A]) = x match { case E00() => ??? ; case E10(x) => x ; case E20(x, y) => x ; case E01(xs @ _*) => xs.head ; case E11(x, ys) => x ; case E21(x, y, zs) => x }

    def gc1[A](x: C[A]) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys) => x ; case F21(x, y, zs) => x }
    def gc2[A](x: C[A]) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys) => x ; case F21(x, y, zs) => x }
    def gc3[A](x: C[A]) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys) => x ; case F21(x, y, zs) => x }
    def gc4[A](x: C[A]) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys) => x ; case F21(x, y, zs) => x }
    def gc5[A](x: C[A]) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys) => x ; case F21(x, y, zs) => x }
    def gc6[A](x: C[A]) = x match { case F00() => ??? ; case F10(x) => x ; case F20(x, y) => x ; case F01(xs @ _*) => xs.head ; case F11(x, ys) => x ; case F21(x, y, zs) => x }


    // def g2(x: Any) = x match { case C00() => 1 ; case C10(x) => 2; case C20(x, y) => 3 ; case C01(xs) => 4 ; case C11(x, ys) => 5 ; case C21(x, y, zs) => 6 }

    // def f10(x: Any) = x match { case C10(x)             => 1 ; case E10(x) => 2 }
    // def f20(x: Any) = x match { case C20(x, y)          => 1 ; case E20(x, y) => 2 }
    // def f01(x: Any) = x match { case C01(xs @ _*)       => 1 ; case E01(xs @ _*) => 2 }
    // def f11(x: Any) = x match { case C11(x, ys @ _*)    => 1 ; case E11(x, ys @ _*) => 2 }
    // def f21(x: Any) = x match { case C21(x, y, zs @ _*) => 1 ; case E21(x, y, zs @ _*) => 2 }
  }
}

