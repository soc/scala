trait A {
  val x: java.lang.Object
  type A2 <: x.type
  val y2: A2

  val q = (y2: x.type)        // fails
  val q2 = ((y2: A2): x.type) // works
}
