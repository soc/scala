/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package javac

import scala.tools.nsc.io.{ AbstractFile, Directory, VirtualDirectory }
import scala.tools.nsc.interpreter.{ AbstractFileClassLoader }
import java.lang.reflect.{ Method => JMethod }
import javax.tools._
import scala.tools.nsc.util.ScalaClassLoader.appLoader
import JavaFileObjects.{ StringFileObject, AbstractFileFileObject, VirtualFileManager }
import JavaCompiler._
import scala.collection.JavaConverters._
import scala.collection.JavaConversions._

// See http://atamur.blogspot.com/2009/10/using-built-in-javacompiler-with-custom.html

object JavaCompiler {
  val freshPkg = {
    var counter = 0
    () => { counter += 1 ; "javapkg$" + counter }
  }
  val freshName = {
    var counter = 0
    () => { counter += 1 ; "javasrc$" + counter }
  }

  val compilerPackage = "repljava"
  val compilerPackageDecl = "package " + compilerPackage + ";\n\n"
  // val freshName = {
  //   var counter = 0
  //   () => { counter += 1 ; "javaSource$" + counter }
  // }
  val defaultOutput = new VirtualDirectory("(VirtualFileManager default)", None)
  val jc            = new JavaCompiler(appLoader, defaultOutput)

  def systemCompiler = ToolProvider.getSystemJavaCompiler
  def newDiagnostics = new DiagnosticCollector[JavaFileObject]()

  private def indent(s: String) = s split "\\n" map ("    " + _ + "\n") mkString

  // Creates string: URI from class name
  def mkSource(name: String, code: String) = """
    |package %s;
    |
    |public class %s {
    |  public static void main(String[] args) {
    |%s
    |  }
    |}
  """.stripMargin.format(compilerPackage, name, indent(code))

  def mkWrappedUnit(code: String) = {
    val name = freshName()
    StringFileObject(name, mkSource(name, code))
  }
  def mkUnit(code: String)               = StringFileObject(freshName(), compilerPackageDecl + code)
  def mkSrc(code: String, wrap: Boolean) = if (wrap) mkWrappedUnit(code) else mkUnit(code)
  def demo: Unit                         = demo("""System.out.println("HI BOB");""")
  def demo(code: String): Unit           = jc.runWrap(code)
}

class JavaCompiler(firstLoader: ClassLoader, val output: VirtualDirectory) {
  val loader = firstLoader match {
    case x: AbstractFileClassLoader => x
    case _                          => new AbstractFileClassLoader(output, firstLoader)
  }
  val compiler    = systemCompiler
  val diag        = newDiagnostics
  val fileManager = new VirtualFileManager(output, loader, compiler.getStandardFileManager(diag, null, null))

  def compiledFiles                        = fileManager.classFiles.toList.sorted
  def load(name: String): Class[_]         = loader.loadClass(name)
  def mainMethod(name: String): JMethod    = load(name).getMethods find (_.getName == "main") orNull
  def runMain(name: String, args: String*) = mainMethod(name).invoke(null, args.toArray)

  def compileWithDiagnostics(files: JavaFileObject*) = {
    val diagnostics = newDiagnostics
    val task        = compiler.getTask(null, fileManager, diagnostics, null, null, files.toList) //asScala.toList)
    val result      = task.call

    diagnostics.getDiagnostics.asScala.toList
  }

  def compileUnit(code: String): Boolean    = compile(mkUnit(code))
  def compileSnippet(code: String): Boolean = compile(mkWrappedUnit(code))
  def compile(files: JavaFileObject*): Boolean =
    compiler.getTask(null, fileManager, null, null, null, files.toList).call

  def run(file: JavaFileObject, args: String*): Boolean = {
    compile(file) && {
      try   { runMain(file.getName, args: _*) ; true }
      catch { case _ => false }
    }
  }
  def runWrap(code: String, args: String*)   = run(mkWrappedUnit(code), args: _*)
  def runNoWrap(code: String, args: String*) = run(mkUnit(code), args: _*)
}
