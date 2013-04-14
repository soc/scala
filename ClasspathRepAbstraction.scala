/* NSC -- new Scala compiler
 * Copyright 2006-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools

import scala.tools.nsc.util.ClassPath._

trait Paths {
  def context: ClassPathContext[_]
  def expandStars(underlying: String): String
  def expandDirs(underlying: String): String
  def defaultPaths: PathSet

  def Literal(underlying: String) = if (underlying == null) NoElem else new Literal(underlying)
  def Star(underlying: String)    = if (underlying == null) NoElem else new Star(underlying)
  def ExtDir(underlying: String)  = if (underlying == null) NoElem else new ExtDir(underlying)

  sealed trait Elem {
    val path: String
    override def toString = path
  }
  object NoElem extends Elem {
    val path = ""
  }
  final class Literal private[Paths] (val path: String) extends Elem {
  }
  final class Star private[Paths] (underlying: String) extends Elem {
    val path = expandStars(underlying)
  }
  final class ExtDir private[Paths] (underlying: String) extends Elem {
    val path = expandDirs(underlying)
  }
  final class PathSet(val boot: Elem, val ext: Elem, val app: Elem) { }
}

trait StandardPaths extends Paths {
  def expandStars(underlying: String) = context.classesInExpandedPath(underlying)
  def expandDirs(underlying: String)  = context.contentsOfDirsInPath(underlying)
  def defaultPaths = new PathSet(
    Literal(sys.props("sun.boot.class.path")),
     ExtDir(sys.props("java.ext.dirs")),
       Star(sys.env.get("CLASSPATH").orNull)
  )
}
