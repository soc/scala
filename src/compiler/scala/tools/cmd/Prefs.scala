package scala.tools
package cmd

import nsc.{ Global, Settings, Interpreter }
import nsc.io.{ Path, File, Directory, Sources }
import nsc.reporters.{ Reporter, ConsoleReporter }
import Directory.Home
import scala.io.Codec
import nsc.util.{ FakePos, Position, ClassPath, SourceFile, BatchSourceFile, ScalaClassLoader, Exceptional }
import ScalaClassLoader.{ URLClassLoader, getSystemLoader, defaultParentClassLoader }

trait Prefs {
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
  lazy val userProps = system.PropertiesMap.user()

  def global[T <: Global](x: T): T      = { initGlobal(x) ; last.global = x ; x }
  def repl[T <: Interpreter](x: T): T   = { initRepl(x) ; last.repl = x ; x }
  def reporter[T <: Reporter](x: T): T  = { initReporter(x) ; last.reporter = x ; x }
  def settings[T <: Settings](x: T): T  = { initSettings(x) ; last.settings = x ; x }

  def newGlobal(s: Settings): Global            = global(new Global(s))
  def newRepl(s: Settings): Interpreter         = repl(new Interpreter(s))
  def newReporter(s: Settings): ConsoleReporter = reporter(new ConsoleReporter(s))
  def newSettings(): Settings                   = settings(new Settings(onErrorMessage))

  def exceptionFormatter: Exceptional.Formatter = Exceptional.Formatter(this)
  def codeSources: Sources = Sources.defaultSources
  def codec: Codec = Codec.default
  def onError(ex: Throwable): Unit = ex.printStackTrace()
  def onErrorMessage(msg: String): Unit = Console println msg
}

trait LowPriorityPrefs {
  self: Prefs.type =>

  implicit def defaultUserPreferences: Prefs = empty
}

object Prefs extends LowPriorityPrefs {
  val PrefsSourceProperty    = "scala.prefs.source"
  val PrefsClassProperty     = "scala.prefs.class"
  val PrefsClasspathProperty = "scala.prefs.classpath"

  private def loadPrefs() = {
    val userProps = system.PropertiesMap.user()
    val urls      = userProps get PrefsClasspathProperty map ClassPath.toURLs getOrElse Nil
    val cl        = new URLClassLoader(urls, getSystemLoader)
    val prefs     = userProps get PrefsClassProperty map cl.createTyped[Prefs] getOrElse Prefs.empty

    prefs.init()
    prefs
  }
  def load() =
    try loadPrefs()
    catch { case x => empty }

  def empty = new Prefs { }
}
