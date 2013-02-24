import scala.reflect.runtime.universe._

// C   Class
// Sn  Singleton
// Al  Alias
// Ab  Abstract Type
// _UB upper bounded
// _EQ eq to
trait Shared[-TZ, T, +TA] {
  // classes
  class C1            { def c1: Any = null }
  class C2 extends C1 { def c2: Any = null }
  class C3 extends C2 { def c3: Any = null }

  val c1 = new C1
  val c2 = new C2
  val c3 = new C3

  // refinement classes
  val rc1 = new C1 { override def c1: String = "c1" }
  val rc2 = new C2 { override def c2: String = "c2" }
  val rc3 = new C3 { override def c3: String = "c3" }

  val c1_eq: c1.type = c1
  val c2_eq: c2.type = c2
  val c3_eq: c3.type = c3

  type C1_type = c1.type
  type C2_type = c2.type
  type C3_type = c3.type

  type C1_type_ub <: c1.type
  type C2_type_ub <: c2.type
  type C3_type_ub <: c3.type

  type C1_eq_type = c1_eq.type
  type C2_eq_type = c2_eq.type
  type C3_eq_type = c3_eq.type

  type C1_eq_type_ub <: c1_eq.type
  type C2_eq_type_ub <: c2_eq.type
  type C3_eq_type_ub <: c3_eq.type

  type C1_alias = C1
  type C2_alias = C2
  type C3_alias = C3

  type C1_ub <: C1
  type C2_ub <: C2
  type C3_ub <: C3

  type C1_alias_ub <: C1_alias
  type C2_alias_ub <: C2_alias
  type C3_alias_ub <: C3_alias

  type TZ_lb >: TZ
  type T_lb  >: T
  type T_ub  <: T
  type TA_ub <: TA
}

trait Markers {
  class MA
  class MB extends MA
  class MC extends MB
  class MD extends MC
  class ME extends MD
}

trait Base1[-BZ, B, +BA] extends Shared[BZ, B, BA] with Markers {
  trait Apple1[A1] {
    type Z1
    trait Apple2[A2] {
      type Z2
      trait Apple3[A3] {
        type Z3
      }
    }
  }
}

trait Base2[-CZ, C, +CA] extends Base1[CZ, C, CA] {
  trait Apple1 extends super.Apple1[C] {
    type Z1 >: CZ
    trait Apple2 extends super.Apple2[C] {
      type Z2 <: CA
      trait Apple3 extends super.Apple3[C] {
        type Z3 = C
      }
    }
  }
  val bippy = new Apple1 {
    val bippy = new Apple2 {
      val bippy = new Apple3 { }
    }
  }
}

object Test extends Base2[Set[Int], Vector[Int], List[Int]] {
  def show[T: TypeTag](x: T) = println(typeOf[T])

  def main(args: Array[String]): Unit = {
    show(bippy)
    show(bippy.bippy)
    println(typeOf[bippy.type])
    // XXX fails.
    // show(bippy.bippy.bippy)

    // XXX fails, fails.
    // println(typeOf[bippy.bippy.type])
    // println(typeOf[bippy.bippy.bippy.type])
    // test/files/run/as-seen-from.scala:110: error: No TypeTag available for Base2.$anon.<refinement>.type
    //     println(typeOf[bippy.bippy.type])
    //                   ^
    // test/files/run/as-seen-from.scala:111: error: No TypeTag available for Base2.$anon.$anon.<refinement>.type
    //     println(typeOf[bippy.bippy.bippy.type])
    //                   ^
    // two errors found
    // println(btpe)
  }
}

