object Test {
  val x = "foo"

  val y1 = Tuple2[x.type, x.type](x,x)
  val y2 = (x : x.type, x : x.type)     // infers (String, String) for y2
  val y3 : (x.type, x.type) = (x, x)
  val y4 : (x.type, x.type) = (x : x.type, x : x.type)
  val y5 : (x.type, x.type) = (x, x) : (x.type, x.type)
  var y6 : (x.type, x.type) = _

  def main(args: Array[String]): Unit = {
    y6 = y1
    // y6 = y2
    y6 = y3
    y6 = y4
    y6 = y5
    y6 = y6
  }
}
