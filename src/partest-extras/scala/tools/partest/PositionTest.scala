package scala.tools.partest

/** A position test can be written as actual source code rather than
 *  an embedded String if the test file utilizes this class. If the
 *  test case begins with these two lines:
 *
       object Test extends scala.tools.partest.PositionTest
       // BEGIN POSITION TEST
 *
 *  Then the remainder of the file will have its ASTs printed
 *  with range positions attached.
 *
 *  See test/files/run/position-test.scala for an example.
 */
class PositionTest extends DirectTest {
  def testMarker             = "POSITION TEST"
  def testStart              = s"BEGIN $testMarker"
  def testEnd                = s"END $testMarker"
  def printAfterPhases       = "parser,typer"

  override def extraSettings = s"-usejavacp -Yrangepos -Xprint-pos -Xprint:$printAfterPhases -d ${testOutput.path}"
  def show()                 = Console.withErr(System.out)(compile())

  /** This looks for the first line containing the test marker,
   *  without regard for anything else.
   */
  def code = {
    val s     = testPath.slurp()
    val lines = s.lines.toList
    val start = lines indexWhere (_ contains testStart)
    val end   = lines indexWhere (_ contains testEnd)

    // When performing this sort of unfortunate "magic" it's
    // super important to give decent error messages if some
    // expectation has been violated in an easily anticipable way.
    assert(start >= 0, s"""
      |Could not find test marker: does the string appear in the file?
      | test source: $testPath
      |start marker: $testStart   (start marker is mandatory)
      |  end marker: $testEnd     (end marker is optional)
      |""".stripMargin)

    val count = if (end >= 0) end - start - 1 else lines.length
    lines drop (start + 1) take count mkString "\n"
  }
}
