object Test extends App {
  import collection.mutable.ListBuffer

  val lb = ListBuffer('a, 'b, 'c, 'd, 'e)

  lb.update(-1, 'u)
}