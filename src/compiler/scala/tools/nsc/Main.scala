/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

/** The main class for NSC, a compiler for the programming
 *  language Scala.
 */
object Main extends Driver with EvalLoop {

  val prompt = Properties.residentPromptString

  def resident(compiler: Global) {
    loop { line =>
      val args = line.split(' ').toList
      val command = new CompilerCommand(args, new Settings(scalacError))
      compiler.reporter.reset()
      new compiler.Run() compile command.files
    }
  }

  override def newCompiler(): Global = Global(settings, reporter)

  override def doCompile(compiler: Global) {
    if (settings.resident.value)
      resident(compiler)
    else super.doCompile(compiler)
  }
}
