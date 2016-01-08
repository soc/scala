import scala.tools.partest.DirectTest

/**
 * Tests that when building a mixed Java/Scala project where
 * - Scala code uses a method from Java which is marked using custom Deprecated annotation
 * - and Java files are not yet built using javac
 * there are no deprecation warnings related to this annotation.
 * On the other hand, fully qualified @java.lang.Deprecated is still handled properly.
 */
object Test extends DirectTest {

  val customDeprecation =
    """
      |import java.lang.annotation.*;
      |import static java.lang.annotation.ElementType.*;
      |
      |@Documented
      |@Retention(RetentionPolicy.RUNTIME)
      |public @interface Deprecated {}
    """.stripMargin

  val javaCode =
    """
      |public class J {
      |  @Deprecated
      |  public static void foo() {}
      |
      |  @java.lang.Deprecated
      |  public static void bar() {}
      |}
    """.stripMargin

  val scalaCode =
    """
      |class S {
      |  J.foo()
      |  J.bar()
      |}
    """.stripMargin

  val sources = newJavaSources(customDeprecation, javaCode) ++ newSources(scalaCode)

  val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
  val compiler = newCompiler("-cp", classpath, "-deprecation", "-Xfatal-warnings")

  sourceFilesToCompiledUnits(compiler)(sources)

  // It's an abstract member which needs to be implemented but we don't use it at all.
  def code: String = ???

  // We expect that after the execution there will be messages from compilation
  // and it's not needed to print here anything else.
  def show(): Unit = {}
}
