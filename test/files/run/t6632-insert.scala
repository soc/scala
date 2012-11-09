object Test extends App {
  import collection.mutable.ListBuffer

  val lb = ListBuffer('a, 'b, 'c, 'd, 'e)

  lb.insert(-1, 'x)
}