/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 */

package scala.tools.nsc
package interpreter

import java.lang.{ ClassLoader => JavaClassLoader }
import java.lang.instrument.{ Instrumentation, ClassDefinition }
import scala.collection.{ mutable, immutable }
import scala.tools.nsc.io.AbstractFile
import scala.tools.util.JVMAgent
import util.ScalaClassLoader
import java.net.URL
import scala.collection.{ mutable, immutable }

/**
 * A class loader that loads files from a {@link scala.tools.nsc.io.AbstractFile}.
 *
 * @author Lex Spoon
 */
class AbstractFileClassLoader(root: AbstractFile, parent: JavaClassLoader)
    extends JavaClassLoader(parent)
    with ScalaClassLoader
{
  override def getBytesForClass(name: String): Array[Byte] = {
    def onull[T](x: T): T = if (x == null) throw new ClassNotFoundException(name) else x
    var file: AbstractFile = root
    val pathParts          = classNameToPath(name) split '/'

    for (dirPart <- pathParts.init) {
      file = file.lookupName(dirPart, true)
      if (file == null)
        return null
    }

    file.lookupName(pathParts.last, false) match {
      case null   => null
      case file   => file
    }
  }

  protected def inst: Instrumentation = JVMAgent.inst

  final def redefineClass(clazz: Class[_], bytes: Array[Byte]): Unit = {
    val newDef = new ClassDefinition(clazz, bytes)
    inst.redefineClasses(newDef)
    classWasRedefined(clazz, bytes)
  }

  def recordClass(name: String, bytes: Array[Byte]): Unit = ()
}


class RedefiningClassLoader(root: AbstractFile, parent: JavaClassLoader) extends AbstractFileClassLoader(root, parent) {
  val classes = new mutable.HashMap[String, Array[Byte]]
  import classes.{ get, getOrElse }

  def redefineClass(name: String, bytes: Array[Byte]): Unit =
    redefineClass(tryToLoadClass(name).get, bytes)

  override def recordClass(name: String, bytes: Array[Byte]): Unit = {
    println("recordClass(%s, %s) where formerly we had %s bytes".format(name, bytes.size, classes.getOrElse(name, Array()).size))

    if (classes contains name) redefineClass(name, bytes)
    else classes(name) = bytes
  }

  override def getBytesForClass(name: String): Array[Byte] =
    getOrElse(name, super.getBytesForClass(name))

  override def findClass(name: String): Class[_] = get(name) match {
    case Some(xs) => defineClass(name, xs, 0, xs.length)
    case _        => super.findClass(name)
  }
  override def classWasRedefined(clazz: Class[_], bytes: Array[Byte]): Unit = {
    classes(clazz.getName()) = bytes
  }
}
