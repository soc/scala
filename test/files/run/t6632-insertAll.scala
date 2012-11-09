object Test extends App {
  import collection.mutable.ListBuffer

  val lb = ListBuffer('a, 'b, 'c, 'd, 'e)

  lb.insertAll(-1, Array('x, 'y, 'z))
}