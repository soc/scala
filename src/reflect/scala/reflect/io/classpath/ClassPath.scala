package scala
package reflect
package io
package classpath

import java.io.File.pathSeparator

/** A collection of paths, and the means to find an array of bytes
 *  associated with each path.
 */
trait ClassPath {
  def paths: Seq[PathRep]
  def packages: Seq[PackageRep]
  def findBytes(path: PathRep): Bytes
}

class PathRepSeq(private[classpath] val arr: Array[String]) extends immutable.IndexedSeq[PathRep] {
  def length = arr.length
  def apply(idx: Int): PathRep = PathRep(arr(idx))
  def iterator = arr.iterator map (s => PathRep(s))
}

class PackageRepSeq(private[classpath] val arr: Array[String]) extends immutable.IndexedSeq[PackageRep] {
  def length = arr.length
  def apply(idx: Int): PackageRep = PackageRep(arr(idx))
  def iterator = arr.iterator map (s => PackageRep(s))
}

object ClassPath {
  private def bootProp = System.getProperty("sun.boot.class.path", "")
  private def extProp  = System.getProperty("java.ext.dirs", "")
  private def appProp  = System.getProperty("java.class.path", "")

  def apply(boot: String = bootProp, ext: String = extProp, app: String = appProp): ClassPath =
    new Impl(singles(split.paths(boot, ext, app)).toList)

  def single(path: PathRep): Single             = Single(path)
  def singles(paths: Seq[PathRep]): Seq[Single] = if (Debug.isParallel) (paths.par map single).seq else paths map single

  /** TODO - the per-single list of packages exists so the singles can be pre-filtered
   *  based on the package of the incoming path, but that's not done yet.
   */
  @tailrec final def findFirstBytes(singles: Seq[Single], path: PathRep): Bytes = (
    if (singles.isEmpty) NoBytes
    else singles.head findBytes path match {
      case NoBytes => findFirstBytes(singles.tail, path)
      case res     => res
    }
  )

  /** Facilities for translating a String into a sequence of PathReps.
   *  Expansion semantics are different for the three types of classpaths.
   *   - boot: each path component is a literal path
   *   -  ext: each path component is a directory, to be expanded to its contents
   *   -  app: each path component is either a literal path or *, where * is expanded as in ext
   */
  object split {
    def apply(path: String): Seq[PathRep] = noEmpty(path split pathSeparator) map (p => PathRep(p))
    def app(cp: String): Seq[PathRep]     = apply(cp) flatMap starOrSelf
    def ext(cp: String): Seq[PathRep]     = apply(cp) flatMap (_.listContainers)
    def boot(cp: String): Seq[PathRep]    = apply(cp)

    def paths(bootCp: String, extCp: String, appCp: String): Seq[PathRep] =
      boot(bootCp) ++ ext(extCp) ++ app(appCp)

    private def noEmpty(xs: Traversable[String]) = xs.toList filterNot (_.trim == "")
    private def starOrSelf(path: PathRep): Seq[PathRep] = path.path match {
      case ""                  => Nil
      case "*"                 => PathRep(".").listContainers
      case p if p endsWith "*" => path.parent.listContainers
      case _                   => path :: Nil
    }
  }

  private class Impl(val singles: List[Single]) extends ClassPath {
    val paths    = singles.flatMap(_.paths).distinct
    val packages = singles.flatMap(_.packages).distinct.sorted

    def findBytes(path: PathRep): Bytes = findFirstBytes(singles, path)
    override def toString = this.summary
  }

  implicit final class ClassPathOps(val cp: ClassPath) extends AnyVal {
    import cp._
    def stats: String = cp match {
      case x: SingleError => ""
      case _              => "%5s files  %4s packages".format(paths.length, packages.length)
    }
    def summary: String = "%30s  %s".format(stats,
      cp match {
        case cp: Single => cp.basis
        case cp: Impl   => "ClassPath(" + cp.singles.size + " roots)"
        case cp         => ""
      }
    )
  }
}
