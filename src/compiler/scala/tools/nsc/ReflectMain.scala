package scala.tools.nsc

import util.ScalaClassLoader
import scala.tools.util.PathResolver

object ReflectMain extends Driver {

  private def classloaderFromSettings(settings: Settings) = {
    val classpath = new PathResolver(settings).result
    ScalaClassLoader.fromURLs(classpath.asURLs, getClass.getClassLoader)
  }

  override def newCompiler(): Global = new ReflectGlobal(settings, reporter, classloaderFromSettings(settings))
}