import scala.tools.partest.DirectTest

/**
 * Tests that when building a mixed Java/Scala project, where Scala code uses deprecated methods from Java file
 * and Java files are not yet built using javac, deprecation warnings are properly reported.
 *
 * The test is written in such a form as when having a directory with both files in neg directory,
 * Java sources are built and the problem is not properly reproduced.
 */
object Test extends DirectTest {

  val javaCode =
    """
      |public class J {
      |
      |  @Deprecated
      |  public static int i = 0;
      |
      |  @Deprecated public class JInner {}
      |
      |  @Deprecated
      |  public J() {}
      |
      |  @Deprecated
      |  public static void foo() {}
      |
      |  // @Deprecated used with a parameter has no effect
      |  public static void bar(@Deprecated int i) {}
      |}
    """.stripMargin

  val scalaCode =
    """
      |class S {
      |  J.foo()
      |  J.bar(J.i)
      |  val j = new J()
      |  val inner = new j.JInner()
      |}
    """.stripMargin

  val sources = newJavaSources(javaCode) ++ newSources(scalaCode)

  val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
  val compiler = newCompiler("-cp", classpath, "-deprecation", "-Xfatal-warnings")

  sourceFilesToCompiledUnits(compiler)(sources)

  // It's an abstract member which needs to be implemented but we don't use it at all.
  def code: String = ???

  // We expect that after the execution there will be messages from compilation
  // and it's not needed to print here anything else.
  def show(): Unit = {}
}
