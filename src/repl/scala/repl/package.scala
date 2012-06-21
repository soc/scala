/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala

import language.implicitConversions
import java.net.URL
import scala.collection.convert._
import scala.io.Codec
import scala.tools.nsc.io.Streamable

/** The main REPL related classes and values are as follows.
 *  In addition to standard compiler classes Global and Settings, there are:
 *
 *  History: an interface for session history.
 *  Completion: an interface for tab completion.
 *  ILoop (formerly InterpreterLoop): The umbrella class for a session.
 *  IMain (formerly Interpreter): Handles the evolving state of the session
 *    and handles submitting code to the compiler and handling the output.
 *  InteractiveReader: how ILoop obtains input.
 *  History: an interface for session history.
 *  Completion: an interface for tab completion.
 */
package object repl extends ReplConfig with ReplStrings with DecorateAsJava with DecorateAsScala {
  type Global = scala.tools.nsc.Global
  type Phase = scala.tools.nsc.Phase
  
  type JFile          = java.io.File
  type JClass         = java.lang.Class[_]
  type JList[T]       = java.util.List[T]
  type JCollection[T] = java.util.Collection[T]
  type JPrintWriter   = java.io.PrintWriter
  type InputStream    = java.io.InputStream
  type OutputStream   = java.io.OutputStream
  type weight         = scala.annotation.implicitWeight

  val IR = Results
  
  def classTag[T](implicit ctag: ClassTag[T]) = ctag
  def typeTag[T](implicit ttag: TypeTag[T])   = ttag
  type ClassTag[T]                            = scala.reflect.ClassTag[T]
  type TypeTag[T]                             = ru.TypeTag[T]
  def typeOf[T: TypeTag] : ru.Type            = typeTag[T].tpe
  def symbolOf[T: TypeTag] : ru.Symbol        = typeOf[T].typeSymbol

  def membersOf[T: TypeTag] : List[(ru.Symbol, ru.Type)] = {
    val tpe = typeOf[T]
    tpe.members.toList map (sym => (sym, sym typeSignatureIn tpe))
  }
  def declsOf[T: TypeTag] : List[(ru.Symbol, ru.Type)] = {
    val tpe = typeOf[T]
    tpe.declarations.toList map (sym => (sym, sym typeSignatureIn tpe))
  }
  def implicitsOf[T: TypeTag] : List[(ru.Symbol, ru.Type)] =
    membersOf[T] filter { case (s, t) => s.isTerm && (s hasFlag ru.Flag.IMPLICIT) }

  def implicitConversionsOf[T: TypeTag] : List[(ru.Symbol, ru.Type)] =
    implicitsOf[T] filter (_._2 <:< typeOf[Function1[_,_]])

  def implicitTermsOf[T: TypeTag] : List[(ru.Symbol, ru.Type)] =
    implicitsOf[T] collect { case (s, t: ru.MethodType) if t.params.isEmpty => ((s, t)) }

  lazy val ru = scala.reflect.runtime.universe
  // new scala.reflect.runtime.JavaUniverse {
  //   override def missingHook(owner: Symbol, name: Name): Symbol = super.missingHook(owner, name)
  //   override def rootClassLoader: ClassLoader = this.getClass.getClassLoader
  //   override def init() = super.init()
  //   override def runtimeMirror(cl: ClassLoader): Mirror = super.runtimeMirror(cl)
  // }
  //
  implicit def postfixOps = language.postfixOps // make all postfix ops in this package compile without warning

  private[repl] implicit def javaCharSeqCollectionToScala(xs: JCollection[_ <: CharSequence]): List[String] =
    xs.asScala.toList map ("" + _)

  private[repl] implicit def enrichAnyRefWithTap[T](x: T) = new TapMaker(x)
  private[repl] def tracing[T](msg: String)(x: T): T = x.tapTrace(msg)
  private[repl] def debugging[T](msg: String)(x: T) = x.tapDebug(msg)

  implicit class ReplInputStreamOps(in: InputStream)(implicit codec: Codec) {
    def bytes(): Array[Byte] = Streamable.bytes(in)
    def slurp(): String      = Streamable.slurp(in)
  }
  implicit class ReplUrlOps(val url: URL) extends AnyVal {
    def slurp(implicit codec: Codec): String = Streamable.slurp(url)
  }
}
