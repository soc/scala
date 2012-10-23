/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package cmd

import nsc.{ Global, Settings, Interpreter }
import nsc.io.{ File, Path, Sources }
import nsc.reporters.{ Reporter, ConsoleReporter }
import scala.io.Codec
import nsc.util.{ ClassPath, ScalaClassLoader, Exceptional }
import Exceptional.safely
import scala.util.Properties.userHome
import system.PropertiesMap

/** A trait for user customization of the compiler and repl.
 *  The mechanism is as follows:
 *  1) A properties called ~/.scalarc (or the user property named in
 *    PrefsConfigFileProperty, presently "scala.prefs.file" must exist
 *    containing this property:
 *  
 *    scala.prefs.class=foo.bar.Prefs  # class must implement scala.tools.cmd.Prefs
 *
 *  Optional additional properties:
 *
 *    scala.prefs.classpath=/some/path # needed if class is not on classpath
 *    scala.prefs.source=/some/file    # needed to recompile source
 *  
 *  If the given class is successfully instantiated and is an
 *  instance of scala.tools.cmd.Prefs, all further activity will be
 *  routed through its hooks.  Any failure leads to an empty Prefs
 *  instance, which has reasonable defaults for everything.
 */
trait Prefs {
  /** Initialization methods: called immediately after creation on the
   *  relevant types.  Defaults obviously do nothing.
   */
  def init(): Unit                           = ()
  def initGlobal(global: Global): Unit       = ()
  def initRepl(repl: Interpreter): Unit      = ()
  def initReporter(reporter: Reporter): Unit = ()
  def initSettings(s: Settings): Unit        = ()
  
  /** The most recently seen instance of each type.
   */
  object last {
    var global: Global = _
    var repl: Interpreter = _
    var reporter: Reporter = _
    var settings: Settings = _
  }
  /** The properties read from .scalarc or PrefsConfigFileProperty.
   */
  lazy val userProps = Prefs.loadUserProperties
  
  /** A "pass-through" interface.  Ideally this would be the only sort of
   *  interface, but we'd be kidding ourselves.
   */
  def global[T <: Global](x: T): T      = { initGlobal(x) ; last.global = x ; x }
  def repl[T <: Interpreter](x: T): T   = { initRepl(x) ; last.repl = x ; x }
  def reporter[T <: Reporter](x: T): T  = { initReporter(x) ; last.reporter = x ; x }
  def settings[T <: Settings](x: T): T  = { initSettings(x) ; last.settings = x ; x }
  
  /** Creation methods.  They call the pass-through interface, which in turn
   *  calls the initialization interface.
   */
  def newGlobal(s: Settings, r: Reporter): Global = global(new Global(s, r))
  def newGlobal(s: Settings): Global              = newGlobal(s, newReporter(s))
  def newRepl(s: Settings): Interpreter           = repl(new Interpreter(s))
  def newReporter(s: Settings): ConsoleReporter   = reporter(new ConsoleReporter(s))
  def newSettings(f: String => Unit): Settings    = settings(new Settings(f))
  def newSettings(): Settings                     = newSettings(onErrorMessage)
  
  /** A Codec can be used to define preferred character encodings. */
  def codec: Codec                              = Codec.default
  
  /** Places to look for source code for rich exception stack traces. */
  def codeSources: Sources                      = Sources.defaultSources
  
  /** How to format exceptions for output, also for rich exceptions. */
  def exceptionFormatter: Exceptional.Formatter = Exceptional.Formatter(this)
  
  /** A method called whenever an exception is caught and the handler
   *  doesn't have a plan.
   */
  def onError(ex: Throwable): Unit              = ex.printStackTrace()
  
  /** A method called when there is an error message and something need
   *  be done with it.  This method is given to the reporters created via
   *  this object.  By default it prints to the console.
   */
  def onErrorMessage(msg: String): Unit         = Console println msg
}

object Prefs {
  val PrefsConfigFileProperty = "scala.prefs.file"
  val PrefsSourceProperty     = "scala.prefs.source"
  val PrefsClassProperty      = "scala.prefs.class"
  val PrefsClasspathProperty  = "scala.prefs.classpath"
  
  def empty: Prefs = new Prefs { }
  
  def userPropertiesFile = (system.props get PrefsConfigFileProperty) match {
    case Some(path)   => File(path)
    case _            => Path(userHome) / ".scalarc" toFile
  }
  def loadUserProperties =
    try PropertiesMap(userPropertiesFile.jfile)
    catch safely(_ => PropertiesMap.empty)

  /** Attempts to find the name of a user defined prefs class and instantiate it.
   *  It may throw exceptions along the way.  If it fails normally, empty prefs.
   */
  private def loadPrefs(): Prefs = {
    import ScalaClassLoader._
    val userProps = loadUserProperties
    val urls      = userProps get PrefsClasspathProperty map ClassPath.toURLs getOrElse Nil
    val cl        = new URLClassLoader(urls, getSystemLoader)
    val prefs     = userProps get PrefsClassProperty map cl.createTyped[Prefs] getOrElse Prefs.empty
    
    prefs.init()
    prefs
  }
  def load() =
    try loadPrefs()
    catch safely(_ => empty)
  
  implicit lazy val userPrefs: Prefs = load()
}

