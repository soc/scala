/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools
package nsc
package settings

import annotation.elidable
import scala.tools.util.PathResolver.Defaults
import scala.collection.mutable
import language.{implicitConversions, existentials}

trait ScalaSettings extends AbsScalaSettings
                       with StandardScalaSettings
                       with Warnings {
  self: MutableSettings =>

  /** Set of settings */
  protected lazy val allSettings = mutable.HashSet[Setting]()

  /** Against my better judgment, giving in to martin here and allowing
   *  CLASSPATH to be used automatically.  So for the user-specified part
   *  of the classpath:
   *
   *  - If -classpath or -cp is given, it is that
   *  - Otherwise, if CLASSPATH is set, it is that
   *  - If neither of those, then "." is used.
   */
  protected def defaultClasspath = sys.env.getOrElse("CLASSPATH", ".")

  /** Enabled under -Xexperimental. */
  protected def experimentalSettings = List[BooleanSetting](YmethodInfer, overrideObjects, overrideVars)

  /** Enabled under -Xfuture. */
  protected def futureSettings = List[BooleanSetting]()

  /** Enabled under -optimise. */
  protected def optimiseSettings = List[BooleanSetting](inline, Xcloselim, Xdce)

  /** Internal use - syntax enhancements. */
  private class EnableSettings[T <: BooleanSetting](val s: T) {
    def enabling(toEnable: List[BooleanSetting]): s.type = s withPostSetHook (_ => toEnable foreach (_.value = s.value))
    def andThen(f: s.T => Unit): s.type                  = s withPostSetHook (setting => f(setting.value))
  }
  private implicit def installEnableSettings[T <: BooleanSetting](s: T) = new EnableSettings(s)

  /** Disable a setting */
  def disable(s: Setting) = allSettings -= s

  val jvmargs  = PrefixSetting("-J<flag>", "-J", "Pass <flag> directly to the runtime system.")
  val defines  = PrefixSetting("-Dproperty=value", "-D", "Pass -Dproperty=value directly to the runtime system.")
  val toolcp   = PathSetting("-toolcp", "Add to the runner classpath.", "")
  val nobootcp = BooleanSetting("-nobootcp", "Do not use the boot classpath for the scala jars.")

  /**
   *  Standard settings
   */
  // argfiles is only for the help message
  val argfiles      = BooleanSetting    ("@<file>", "A text file containing compiler arguments (options and source files)")
  val classpath     = PathSetting       ("-classpath", "Specify where to find user class files.", defaultClasspath) withAbbreviation "-cp"
  val d             = OutputSetting     (outputDirs, ".")
  val nospecialization = BooleanSetting    ("-no-specialization", "Ignore @specialize annotations.")
  val language      = MultiStringSetting("-language", "feature", "Enable one or more language features.")

  /**
   * -X "Advanced" settings
   */
  val Xhelp         = BooleanSetting    ("-X", "Print a synopsis of advanced options.")
  val checkInit     = BooleanSetting    ("-Xcheckinit", "Wrap field accessors to throw an exception on uninitialized access.")
  val noassertions  = BooleanSetting    ("-Xdisable-assertions", "Generate no assertions or assumptions.")
  val elidebelow    = IntSetting        ("-Xelide-below", "Calls to @elidable methods are omitted if method priority is lower than argument",
                                                elidable.MINIMUM, None, elidable.byName get _)
  val noForwarders  = BooleanSetting    ("-Xno-forwarders", "Do not generate static forwarders in mirror classes.")
  val genPhaseGraph = StringSetting     ("-Xgenerate-phase-graph", "file", "Generate the phase graphs (outputs .dot files) to fileX.dot.", "")
  val XlogImplicits = BooleanSetting    ("-Xlog-implicits", "Show more detail on why some implicits are not applicable.")
  val logImplicitConv = BooleanSetting  ("-Xlog-implicit-conversions", "Print a message whenever an implicit conversion is inserted.")
  val logReflectiveCalls = BooleanSetting("-Xlog-reflective-calls", "Print a message when a reflective method call is generated")
  val logRuntimeSplices = BooleanSetting("-Xlog-runtime-splices", "Print a message when Expr.eval or Expr.value cannot be resolved statically.")
  val logFreeTerms  = BooleanSetting    ("-Xlog-free-terms", "Print a message when reification creates a free term.")
  val logFreeTypes  = BooleanSetting    ("-Xlog-free-types", "Print a message when reification resorts to generating a free type.")
  val maxClassfileName = IntSetting     ("-Xmax-classfile-name", "Maximum filename length for generated classes", 255, Some((72, 255)), _ => None)
  val Xmigration28  = BooleanSetting    ("-Xmigration", "Warn about constructs whose behavior may have changed between 2.7 and 2.8.")
  val nouescape     = BooleanSetting    ("-Xno-uescape", "Disable handling of \\u unicode escapes.")
  val Xnojline      = BooleanSetting    ("-Xnojline", "Do not use JLine for editing.")
  val Xverify       = BooleanSetting    ("-Xverify", "Verify generic signatures in generated bytecode (asm backend only.)")
  val Xprint        = PhasesSetting     ("-Xprint", "Print out program after")
  val writeICode    = PhasesSetting     ("-Xprint-icode", "Log internal icode to *.icode files after", "icode")
  val Xprintpos     = BooleanSetting    ("-Xprint-pos", "Print tree positions, as offsets.")
  val printtypes    = BooleanSetting    ("-Xprint-types", "Print tree types (debugging option).")
  val prompt        = BooleanSetting    ("-Xprompt", "Display a prompt after each error (debugging option).")
  val resident      = BooleanSetting    ("-Xresident", "Compiler stays resident: read source filenames from standard input.")
  val script        = StringSetting     ("-Xscript", "object", "Treat the source file as a script and wrap it in a main method.", "")
  val mainClass     = StringSetting     ("-Xmain-class", "path", "Class for manifest's Main-Class entry (only useful with -d <jar>)", "")
  val Xshowcls      = StringSetting     ("-Xshow-class", "class", "Show internal representation of class.", "")
  val Xshowobj      = StringSetting     ("-Xshow-object", "object", "Show internal representation of object.", "")
  val showPhases    = BooleanSetting    ("-Xshow-phases", "Print a synopsis of compiler phases.")
  val sourceReader  = StringSetting     ("-Xsource-reader", "classname", "Specify a custom method for reading source files.", "")

  /** Compatibility stubs for options whose value name did
   *  not previously match the option name.
   */
  def XO = optimise
  def debuginfo = g
  def nowarnings = nowarn
  def outdir = d
  def printLate = print

  /**
   * -Y "Private" settings
   */
  val overrideObjects = BooleanSetting    ("-Yoverride-objects", "Allow member objects to be overridden.")
  val overrideVars    = BooleanSetting    ("-Yoverride-vars", "Allow vars to be overridden.")
  val Yhelp           = BooleanSetting    ("-Y", "Print a synopsis of private options.")
  val check           = PhasesSetting     ("-Ycheck", "Check the tree at the end of")
  val Yshow           = PhasesSetting     ("-Yshow", "(Requires -Xshow-class or -Xshow-object) Show after")
  val Xcloselim       = BooleanSetting    ("-Yclosure-elim", "Perform closure elimination.")
  val Ycompacttrees   = BooleanSetting    ("-Ycompact-trees", "Use compact tree printer when displaying trees.")
  val Xdce            = BooleanSetting    ("-Ydead-code", "Perform dead code elimination.")
  val debug           = BooleanSetting    ("-Ydebug", "Increase the quantity of debugging output.")
  val termConflict    = ChoiceSetting     ("-Yresolve-term-conflict", "strategy", "Resolve term conflicts", List("package", "object", "error"), "error")
  val inline          = BooleanSetting    ("-Yinline", "Perform inlining when possible.")
  val YinlinerWarnings= BooleanSetting    ("-Yinline-warnings", "Emit inlining warnings. (Normally surpressed due to high volume)")
  val log             = PhasesSetting     ("-Ylog", "Log operations during")
  val Ylogcp          = BooleanSetting    ("-Ylog-classpath", "Output information about what classpath is being applied.")
  val Ynogenericsig   = BooleanSetting    ("-Yno-generic-signatures", "Suppress generation of generic signatures for Java.")
  val noimports       = BooleanSetting    ("-Yno-imports", "Compile without importing scala.*, java.lang.*, or Predef.")
  val nopredef        = BooleanSetting    ("-Yno-predef", "Compile without importing Predef.")
  val noAdaptedArgs   = BooleanSetting    ("-Yno-adapted-args", "Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.")
  val Yprofile        = PhasesSetting     ("-Yprofile", "(Requires jvm -agentpath to contain yjgpagent) Profile CPU usage of")
  val Yrecursion      = IntSetting        ("-Yrecursion", "Set recursion depth used when locking symbols.", 0, Some((0, Int.MaxValue)), (_: String) => None)
  val Xshowtrees      = BooleanSetting    ("-Yshow-trees", "(Requires -Xprint:) Print detailed ASTs in formatted form.")
  val XshowtreesCompact
                      = BooleanSetting    ("-Yshow-trees-compact", "(Requires -Xprint:) Print detailed ASTs in compact form.")
  val XshowtreesStringified
                      = BooleanSetting    ("-Yshow-trees-stringified", "(Requires -Xprint:) Print stringifications along with detailed ASTs.")
  val Yshowsyms       = BooleanSetting    ("-Yshow-syms", "Print the AST symbol hierarchy after each phase.")
  val Yshowsymkinds   = BooleanSetting    ("-Yshow-symkinds", "Print abbreviated symbol kinds next to symbol names.")
  val skip            = PhasesSetting     ("-Yskip", "Skip")
  val Ygenjavap       = StringSetting     ("-Ygen-javap", "dir", "Generate a parallel output directory of .javap files.", "")
  val Ydumpclasses    = StringSetting     ("-Ydump-classes", "dir", "Dump the generated bytecode to .class files (useful for reflective compilation that utilizes in-memory classloaders).", "")
  val Ystatistics     = BooleanSetting    ("-Ystatistics", "Print compiler statistics.") andThen (util.Statistics.enabled = _)
  val stopAfter       = PhasesSetting     ("-Ystop-after", "Stop after")
  val stopBefore      = PhasesSetting     ("-Ystop-before", "Stop before")
  val Yrangepos       = BooleanSetting    ("-Yrangepos", "Use range positions for syntax trees.")
  val Yreifycopypaste = BooleanSetting    ("-Yreify-copypaste", "Dump the reified trees in copypasteable representation.")
  val Yreplsync       = BooleanSetting    ("-Yrepl-sync", "Do not use asynchronous code for repl startup")
  val Ynotnull        = BooleanSetting    ("-Ynotnull", "Enable (experimental and incomplete) scala.NotNull.")
  val YmethodInfer    = BooleanSetting    ("-Yinfer-argument-types", "Infer types for arguments of overriden methods.")
  val noSelfCheck     = BooleanSetting    ("-Yno-self-type-checks", "Suppress check for self-type conformance among inherited members.")
  val exposeEmptyPackage = BooleanSetting("-Yexpose-empty-package", "Internal only: expose the empty package.").internalOnly()

  /** Area-specific debug output.
   */
  val Yinferdebug             = BooleanSetting("-Yinfer-debug", "Trace type inference and implicit search.")
  val Yissuedebug             = BooleanSetting("-Yissue-debug", "Print stack traces when a context issues an error.")
  val YmacrodebugLite         = BooleanSetting("-Ymacro-debug-lite", "Trace essential macro-related activities.")
  val YmacrodebugVerbose      = BooleanSetting("-Ymacro-debug-verbose", "Trace all macro-related activities: compilation, generation of synthetics, classloading, expansion, exceptions.")
  val Ypmatdebug              = BooleanSetting("-Ypmat-debug", "Trace all pattern matcher activity.")
  val Yposdebug               = BooleanSetting("-Ypos-debug", "Trace position validation.")
  val Yreifydebug             = BooleanSetting("-Yreify-debug", "Trace reification.")
  val Yrepldebug              = BooleanSetting("-Yrepl-debug", "Trace all repl activity.") andThen (interpreter.replProps.debug setValue _)
  val Ytyperdebug             = BooleanSetting("-Ytyper-debug", "Trace all type assignments.")

  /** Groups of Settings.
   */
  val future        = BooleanSetting("-Xfuture", "Turn on future language features.") enabling futureSettings
  val optimise      = BooleanSetting("-optimise", "Generates faster bytecode by applying optimisations to the program") withAbbreviation "-optimize" enabling optimiseSettings
  val Xexperimental = BooleanSetting("-Xexperimental", "Enable experimental extensions.") enabling experimentalSettings

  // Feature extensions
  val Xmacros                 = BooleanSetting("-Xmacros", "Enable macros.") // [Martin] Can be retired now.
  val XmacroSettings          = MultiStringSetting("-Xmacro-settings", "option", "Custom settings for macros.")
  val XmacroPrimaryClasspath  = PathSetting("-Xmacro-primary-classpath", "Classpath to load macros implementations from, defaults to compilation classpath (aka \"library classpath\".", "")
  val XmacroFallbackClasspath = PathSetting("-Xmacro-fallback-classpath", "Classpath to load macros implementations from if they cannot be loaded from library classpath.", "")
}
