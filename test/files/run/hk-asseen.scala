
abstract class hkm[Z, +DD[X]] {
  trait Builder[-Elem, +To >: DD[Z]]
  trait GenBase[+A] { def f: A }
  trait GenBaseLike[+A, +Repr] extends GenBase[A] with Bippizable[A, Bippy.type]
  trait GenComp[+CC[X] <: GenBase[X]] {
    protected[this] type Coll = CC[_]
    def build[A]: Builder[A, DD[Z]]
  }

  trait Base[+A] extends GenBase[A]
  object Bippy extends BippyMarker
  trait BippyMarker
  trait Bippizable[+A, +BipRepr <: BippyMarker]
  trait View[+A, +Coll] extends ViewLike[A, Coll, View[A, Coll]]
  trait ViewLike[+A, +Coll, +This <: View[A, Coll] with ViewLike[A, Coll, This]] extends BaseLike[A, This]

  trait BaseLike[+A, +Repr] {
    protected[this] type Self = Repr
    def f: A
    def g: A
    def h: Z
  }
}
object hk extends hkm[Int, List] { }

object Test {
  import scala.reflect.api.Universe
  import scala.tools.nsc.Global
  implicit def upgradeType[G1 <: Universe, G2 <: Global](tp: G1#Type): G2#Type        = tp.asInstanceOf[G2#Type]
  implicit def upgradeSymbol[G1 <: Universe, G2 <: Global](sym: G1#Symbol): G2#Symbol = sym.asInstanceOf[G2#Symbol]

  def main(args: Array[String]): Unit = {
    import scala.reflect.runtime.universe._
    val members = typeOf[hkm[Any, Any]].members filter (_.isType)
    members foreach (_.typeParams foreach (_.typeSignature))
    members.map(s => s.defStringSeenAs(s.info)).toList.sorted foreach println
    members.map(s => s.defStringSeenAs(s.info.asSeenFrom(typeOf[hk.type], typeOf[hkm[Any,Any]].typeSymbol))).toList.sorted foreach println
  }
}
