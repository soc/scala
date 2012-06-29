/* NSC -- new Scala compiler
 * Copyright 2006-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package util

import java.lang.instrument._

class JVMAgentImpl {
  var inst: Instrumentation = null

  val manifestText = """
    |Premain-Class: scala.tools.util.JVMAgent
    |Boot-Class-Path: scala-library.jar:scala-compiler.jar
    |Can-Redefine-Classes: true
    """.trim.stripMargin

  def generateManifest() = println(manifestText)

  def premain(agentArgs: String, inst: Instrumentation): Unit = {
    this.inst = inst
    println("JVMAgent.premain(%s, %s)".format(agentArgs, inst))
  }

}

object JVMAgent extends JVMAgentImpl {
  def main(args: Array[String]): Unit = generateManifest()
}

//
// /* NSC -- new Scala compiler
//  * Copyright 2006-2010 LAMP/EPFL
//  * @author  Paul Phillips
//  */
//
// package scala.tools
// package util
//
// import java.lang.{ ClassLoader => JavaClassLoader }
// import java.lang.instrument._
// import nsc.util.ScalaClassLoader
// import java.lang.instrument.ClassDefinition
// import scala.collection.mutable.HashMap
//
// // class JVMAgent { }
//
// object JVMAgent {
//   var inst: Instrumentation = null
//
//   val manifestText = """
//     |Premain-Class: scala.tools.util.JVMAgent
//     |Boot-Class-Path: scala-library.jar:scala-compiler.jar
//     |Can-Redefine-Classes: true
//     """.trim.stripMargin
//
//   def generateManifest() = println(manifestText)
//
//   def premain(agentArgs: String, inst: Instrumentation): Unit = {
//     this.inst = inst
//     println("JVMAgent.premain(%s, %s)".format(agentArgs, inst))
//   }
//
//   def main(args: Array[String]): Unit = generateManifest()
// }
//
// trait ClassLoaderCache extends ScalaClassLoader {
//   val classes = new HashMap[String, Array[Byte]]
//
//   def recordClass(name: String, bytes: Array[Byte]): Unit = {
//     if (classes contains name) redefineClass(name, bytes)
//     else classes(name) = bytes
//   }
//
//   override def getBytesForClass(name: String): Array[Byte] =
//     classes.getOrElse(name, super.getBytesForClass(name))
//
//   override def classWasRedefined(clazz: Class[_], bytes: Array[Byte]): Unit = {
//     classes(clazz.getName()) = bytes
//   }
// }
