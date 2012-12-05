object Test {
  type Id[X] = X

  def main(args: Array[String]) {
    val a: Id[Option[Int]] = None

    a match {
      case None => println("Nothing")
      case Some(x) => println(x)
    }
  }
}
