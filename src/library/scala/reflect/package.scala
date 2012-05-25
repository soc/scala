package scala

package object reflect {

  import ReflectionUtils._
  import scala.compat.Platform.EOL

  // !!! This was a val; we can't throw exceptions that aggressively without breaking
  // non-standard environments, e.g. google app engine.  I made it a lazy val, but
  // I think it would be better yet to throw the exception somewhere else - not during
  // initialization, but in response to a doomed attempt to utilize it.

  // todo. default mirror (a static object) might become a source for memory leaks (because it holds a strong reference to a classloader)!
  lazy val mirror: api.Mirror =
    try mkMirror(defaultReflectionClassLoader)
    catch {
      case ex: UnsupportedOperationException =>
        new DummyMirror(defaultReflectionClassLoader)
    }

  private[scala] def mirrorDiagnostics(cl: ClassLoader): String = """
    |
    | This error has happened because `scala.reflect.runtime.package` located in
    | scala-compiler.jar cannot be loaded. Classloader you are using is:
    | %s.
    |
    | For the instructions for some of the situations that might be relevant
    | visit our knowledge base at https://gist.github.com/2391081.
  """.stripMargin('|').format(show(cl))

  def mkMirror(classLoader: ClassLoader): api.Mirror = {
    val coreClassLoader = getClass.getClassLoader
    val instance = invokeFactoryOpt(coreClassLoader, "scala.reflect.runtime.package", "mkMirror", classLoader)
    instance match {
      case Some(x: api.Mirror) => x
      case Some(_) => throw new UnsupportedOperationException("Available scala reflection implementation is incompatible with this interface." + mirrorDiagnostics(coreClassLoader))
      case None => throw new UnsupportedOperationException("Scala reflection not available on this platform." + mirrorDiagnostics(coreClassLoader))
    }
  }

  // ArrayTag trait is defined separately from the mirror
  // ErasureTag trait is defined separately from the mirror
  // ConcreteErasureTag trait is defined separately from the mirror
  // ClassTag class is defined separately from the mirror
  type TypeTag[T]          = scala.reflect.mirror.TypeTag[T]

  // ClassTag object is defined separately from the mirror
  lazy val TypeTag         = scala.reflect.mirror.TypeTag

  def arrayTagToClassManifest[T](tag: ArrayTag[T]): ClassManifest[T] = TagInterop.arrayTagToClassManifest[T](tag)
}
