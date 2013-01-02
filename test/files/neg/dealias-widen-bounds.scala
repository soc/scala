trait A {
  // type T1 <: AnyRef
  // type T2 <: T1
  // type T3 <: T2

  // val x: T1
  // val y: x.type
  // val x2: T2 with y.type
  // val y2: x2.type
  // val x3: T3 with x2.type
  // val y3: x3.type

  // type A1 = x3.type
  // type A2 = y3.type

  // def f1[T <: A1, U <: A2](x: T, y: U) = List(x, y)

  val x: List[String]
  // type A1 = x.type
  type A2 <: x.type

  // val y1: A1
  val y2: A2

  // println(y1: x.type) // works
  val q = (y2: x.type)
  val q2 = ((y2: A2): x.type)
  // println(y2: x.type) // fails
  // (x3: A1)

  // def f1[T <: A1](x: T) = List[x3.type](x)
  // def f2[U <: A2](y: U) = List[x3.type](y)
  // def f3[T <: A1, U <: A2](x: T, y: U) = List[x3.type](x, y)
}
