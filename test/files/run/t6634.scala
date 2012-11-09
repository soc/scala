object Test extends App {
  import collection.mutable.ListBuffer

  def newLB = ListBuffer('a, 'b, 'c, 'd, 'e)

  val lb0 = newLB

  try {
    lb0.remove(5, 0)
  } catch {
    case ex: IndexOutOfBoundsException => println(ex)
  }

  val lb1 = newLB

  try {
    lb1.remove(6, 6)
  } catch {
    case ex: IndexOutOfBoundsException => println(ex)
  }

  // buffer lb1 should not be corrupted after calling remove with invalid arguments
  val replStr = scala.runtime.ScalaRunTime.replStringOf(lb1, 100)
  if (replStr == "ListBuffer('a, 'b, 'c, 'd, 'e)\n") ()
  else println("  replStringOf FAILED: " + replStr)
    
  val len = lb1.length
  if (len == 5) ()
  else println("  length FAILED: " + len)

  val lb2 = newLB

  try {
    lb2.remove(1, 9)
  } catch {
    case ex: IndexOutOfBoundsException => println(ex)
  }

  val lb3 = newLB

  try {
    lb3.remove(-1, 1)
  } catch {
    case ex: IndexOutOfBoundsException => println(ex)
  }

  val lb4 = newLB

  try {
    lb4.remove(1, -1)
  } catch {
    case ex: IllegalArgumentException => println(ex)
  }
}