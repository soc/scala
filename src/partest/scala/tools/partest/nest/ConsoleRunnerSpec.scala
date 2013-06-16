package scala.tools.partest.nest

import scala.tools.cmd.{ CommandLine, Interpolation, Meta, Reference, Spec }

trait ConsoleRunnerSpec extends Spec with Meta.StdOpts with Interpolation {
  def referenceSpec       = ConsoleRunnerSpec
  def programInfo         = Spec.Info(
      "console-runner",
      "Usage: NestRunner [options] [test test ...]",
      "scala.tools.partest.nest.ConsoleRunner")

  heading("Test categories:")
  val optAll          = "all"          / "run all tests"                     --?
  val optPos          = "pos"          / "run compilation tests (success)"   --?
  val optNeg          = "neg"          / "run compilation tests (failure)"   --?
  val optRun          = "run"          / "run interpreter and backend tests" --?
  val optJvm          = "jvm"          / "run JVM backend tests"             --?
  val optRes          = "res"          / "run resident compiler tests"       --?
  val optScalacheck   = "scalacheck"   / "run ScalaCheck tests"              --?
  val optInstrumented = "instrumented" / "run instrumented tests"            --?
  val optPresentation = "presentation" / "run presentation compiler tests"   --?

  heading("Other options:")
  val optPack         = "pack"         / "pick compiler/reflect/library in build/pack, and run all tests" --?
  val optGrep         = "grep"         / "run all tests whose source file contains the expression given to grep"        --|
  val optFailed       = "failed"       / "run only those tests that failed during the last run" --?
  val optUpdateCheck  = "update-check" / "instead of failing tests with output change, update checkfile (use with care!)" --?
  val optVerbose      = "verbose"      / "show progress information" --?
  val optTerse        = "terse"        / "" --?
  val optBuildPath    = "buildpath"    / "set (relative) path to build jars (ex.: --buildpath build/pack)"        --|
  val optClassPath    = "classpath"    / "set (absolute) path to build classes"        --|
  val optSourcePath   = "srcpath"      / "set (relative) path to test source files (ex.: --srcpath pending)"        --|
  val optDebug        = "debug"        / "enable debugging output" --?

  //println(scala.tools.partest.utils.Properties.versionString)
  //println("maintained by Philipp Haller (EPFL)")


  val optShowDiff     = "show-diff"    / "" --?
  val optShowLog      = "show-log"     / "" --?
  val optSelfTest     = "self-test"    / "" --?
  val optVersion      = "version"      / "" --?
  val optAnsi         = "ansi"         / "" --?
  val optHelp         = "help"         / "" --?
  //standardopts!


  val optBuildmanager = "buildmanager" / "" --?
  val optScalap       = "scalap"       / "" --?
  val optSpecialized  = "specialized"  / "" --?
  val optAnt          = "ant"          / "" --?
  val optTimeout     = "timeout"      / ""        --|

}

object ConsoleRunnerSpec extends ConsoleRunnerSpec with Reference {
  type ThisCommandLine = CommandLine
  def creator(args: List[String]): ThisCommandLine = new CommandLine(ConsoleRunnerSpec, args)
}
