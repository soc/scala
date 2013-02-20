import s.*;

class J_1 {
	HMap<String, String> map = new HMap<String, String>();

	Fun1<Tup2<String, String>, Integer> f =
	  new AbFun1<Tup2<String, String>, Integer>() {
	    public Integer apply(Tup2<String, String> s) {
	      return s._1().length();
	    }
    };

	// scala.collection.Seq<Integer> counts =
	  // map.groupBy(f).keys().toList();
}

// class HMap[K, +V]()

// trait Fun1[-T, +R] {
//   def apply(v1: T1): R
//   def compose[A](g: A => T1): A => R = { x => apply(g(x)) }
//   def andThen[A](g: R => A): T1 => A = { x => g(apply(x)) }
// }
// case class Tup2[+T1, T2](x1: T1, x2: T2)
// abstract class AbFun1[-T, +R] extends Fun1[T, R] { }
