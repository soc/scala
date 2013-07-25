package x

object Test {
  def to_s(x: Any) = x.getClass.getName

  def f1 = {
    import s1._
    val a  = new A
    val b  = new a.B
    val c  = new b.C
    val d  = new c.D
    val e  = new d.E
    val fc = d.F()
    val fo = d.F
    val g  = new fc.G

    List(g, fo, fc, e, d, c, b, a) map to_s
  }
  def f2 = {
    import s2._
    val a  = A
    val b  = new a.B
    val c  = new b.C
    val d  = new c.D
    val e  = new d.E
    val fc = d.F()
    val fo = d.F
    val g  = new fc.G

    List(g, fo, fc, e, d, c, b, a) map to_s
  }

  def f3 = {
    import s3._
    val a  = new A
    val b  = a.B
    val c  = new b.C
    val d  = new c.D
    val e  = new d.E
    val fc = d.F()
    val fo = d.F
    val g  = new fc.G

    List(g, fo, fc, e, d, c, b, a) map to_s
  }
  def f4 = {
    import s4._
    val a  = A
    val b  = a.B
    val c  = new b.C
    val d  = new c.D
    val e  = new d.E
    val fc = d.F()
    val fo = d.F
    val g  = new fc.G

    List(g, fo, fc, e, d, c, b, a) map to_s
  }

  def main(args: Array[String]): Unit = {
    for (((c1, c2), (c3, c4)) <- (f1 zip f2, f3 zip f4).zipped)
      println(List(c1, c2, c3, c4).mkString("\n") + "\n")
  }
}
