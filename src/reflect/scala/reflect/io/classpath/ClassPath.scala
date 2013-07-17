package scala
package reflect
package io
package classpath

import scala.collection.mutable

trait Common {
  def reps: Seq[FileRep]
  def packages: Seq[PackageRep]
  def findBytes(path: String): Array[Byte]
  def summary: String
}
trait ClassPath extends Common {
  def singles: Seq[Single]
}

final case class PackageRep(name: String) extends AnyVal {
  override def toString = s"package $name"
}

class ClassPathImpl(val classpath: String, val singles: Seq[Single]) extends ClassPath {
  def repsSize     = singles map (_.reps.size) sum
  def packagesSize = packages.size
  def reps         = singles.flatMap(_.reps).distinct
  def packages     = singles.flatMap(_.packages).distinct

  // def classes  = singles flatMap (_.classes)
  // def packages = singles flatMap (_.packages)

  // private val router = {
  //   val packageToSingles = mutable.Map[PackageRep, List[Single]]() withDefaultValue Nil
  //   for (s <- singles; pkg <- s.packages)
  //     packageToSingles(pkg) ::= s

  //   packageToSingles.toMap withDefaultValue Nil
  // }
  // private def findSingles(path: String) = router(packageOf(path))

  // def findFile(path: String): FileRep = {
  //   def loop(xs: List[Single]): FileRep = xs match {
  //     case Nil     => NoFileRep
  //     case s :: ss => s findFile path match { case NoFileRep => loop(ss) ; case result => result }
  //   }
  //   loop(findSingles(path))
  // }
  def findBytes(path: String): Bytes = {
    def loop(xs: Seq[Single]): Bytes = (
      if (xs.isEmpty) NoBytes
      else xs.head findBytes path match {
        case NoBytes => loop(xs.tail)
        case result  => result
      }
    )
    loop(singles)
    // loop(findSingles(path))
  }
  def urls                 = validSingles map (_.uri.toURL)
  def summary              = s"ClassPath[${singles.size} roots, $repsSize paths, $packagesSize packages]"
  private def validSingles = singles filterNot (s => s.isError || s.isEmpty)
  private def singles_s    = validSingles mkString "\n"
  override def toString    = summary
}

object ClassPath {
  private def boot = System getProperty "sun.boot.class.path"
  private def ext  = System getProperty "java.ext.dirs"
  private def app  = System getProperty "java.class.path"

  def expandApp(cp: String): Seq[FileRep]    = split(cp) flatMap starOrSelf
  def expandExtDir(cp: String): Seq[FileRep] = split(cp) flatMap (f => fileRep(f).containersIn)
  def expandBoot(cp: String): Seq[FileRep]   = split(cp) map fileRep

  def defaultSingles = {
    val p1 = if (boot eq null) Nil else expandBoot(boot)
    val p2 = if (ext eq null) Nil else expandExtDir(ext)
    val p3 = if (app eq null) Nil else expandApp(app)

    Single singles p1 ++ p2 ++ p3
  }

  def default = new ClassPathImpl(join(boot, ext, app), defaultSingles)

  def apply(cp: String): ClassPath    = new ClassPathImpl(cp, Single singles expandApp(cp))
  def split(path: String): Seq[jFile] = noEmpty(path split pathSeparator) map file
  def join(paths: String*): String    = noEmpty(paths) mkString pathSeparator

  private def noEmpty(xs: Traversable[String]) = xs.toList filterNot (_.trim == "")

  def starOrSelf(path: jFile): Seq[FileRep] = path.getPath match {
    case ""                  => Nil
    case "*"                 => fileRep(".").containersIn
    case p if p endsWith "*" => fileRep(path.getParentFile).containersIn
    case _                   => fileRep(path) :: Nil
  }
}
