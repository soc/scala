/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package backend

import util.{ClassPath,SimpleClassPath,MergedClassPath,DeltaClassPath}
import scala.tools.util.{ PathResolver }

trait JavaPlatform extends Platform {
  import global._
  import definitions._

  private var currentClassPath: Option[SimpleClassPath] = None

  // def classPath: SimpleClassPath = {
  def classPath: SimpleClassPath = {
    if (currentClassPath.isEmpty) currentClassPath = Some(new PathResolver(settings).result)
    currentClassPath.get
  }

  /** Update classpath with a substituted subentry */
  def updateClassPath(subst: Map[SimpleClassPath, SimpleClassPath]) =
    currentClassPath foreach (p => currentClassPath = Some(new DeltaClassPath(p, subst)))

  def rootLoader = new loaders.PackageLoader(classPath)

  private def classEmitPhase =
    if (settings.isBCodeActive) genBCode
    else genASM

  def platformPhases = List(
    flatten,        // get rid of inner classes
    classEmitPhase  // generate .class files
  )

  lazy val externalEquals          = getDecl(BoxesRunTimeClass, nme.equals_)
  lazy val externalEqualsNumNum    = getDecl(BoxesRunTimeClass, nme.equalsNumNum)
  lazy val externalEqualsNumChar   = getDecl(BoxesRunTimeClass, nme.equalsNumChar)
  lazy val externalEqualsNumObject = getDecl(BoxesRunTimeClass, nme.equalsNumObject)

  /** We could get away with excluding BoxedBooleanClass for the
   *  purpose of equality testing since it need not compare equal
   *  to anything but other booleans, but it should be present in
   *  case this is put to other uses.
   */
  def isMaybeBoxed(sym: Symbol) = {
    (sym == ObjectClass) ||
    (sym == JavaSerializableClass) ||
    (sym == ComparableClass) ||
    (sym isNonBottomSubClass BoxedNumberClass) ||
    (sym isNonBottomSubClass BoxedCharacterClass) ||
    (sym isNonBottomSubClass BoxedBooleanClass)
  }

  def newClassLoader(bin: AbstractFile): loaders.SymbolLoader =
    new loaders.ClassfileLoader(bin)

  def doLoad(cls: ClassRep): Boolean = true

  def needCompile(bin: AbstractFile, src: AbstractFile) =
    src.lastModified >= bin.lastModified
}
