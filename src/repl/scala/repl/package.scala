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
  
  lazy val ru = scala.reflect.runtime.universe
  // new scala.reflect.runtime.JavaUniverse {
  //   override def missingHook(owner: Symbol, name: Name): Symbol = super.missingHook(owner, name)
  //   override def rootClassLoader: ClassLoader = this.getClass.getClassLoader
  //   override def init() = super.init()
  //   override def runtimeMirror(cl: ClassLoader): Mirror = super.runtimeMirror(cl)
  // }
  //
  import ru._

  type ClassTag[T]                            = scala.reflect.ClassTag[T]
  type TypeTag[T]                             = ru.TypeTag[T]

  def classTag[T](implicit ctag: ClassTag[T]) = ctag
  def typeTag[T](implicit ttag: TypeTag[T])   = ttag
  def typeOf[T: TypeTag] : ru.Type            = typeTag[T].tpe
  def symbolOf[T: TypeTag] : ru.Symbol        = typeOf[T].typeSymbol

  lazy val Function1Symbol = symbolOf[Function1[_,_]]
  lazy val Function1Type   = typeOf[Function1[_,_]]

  class ImplicitConversion(val symbol: Symbol, val tpe: Type, val from: Type, val to: Type) {
    def matches(from0: Type, to0: Type) = (from0 <:< from) && (to <:< to0)

    private def defString = (symbol: Any) match {
      case x: scala.reflect.internal.Symbols#Symbol =>
        implicit def fixtype[U](x: Type): U = x.asInstanceOf[U]
        Some(x.defStringSeenAs(tpe))
      case _ =>
        None
    }

    override def toString = defString getOrElse {
      "" + symbol + symbol.typeSignature
    }
  }
  class ImplicitMatcher[Owner: TypeTag]() {
    val tpe = typeOf[Owner]
    val conversions = implicitsIn[Owner] flatMap { sym =>
      val symtpe = sym typeSignatureIn tpe
      for ((from, to) <- conversionTypes(symtpe)) yield
        new ImplicitConversion(sym, symtpe, from, to)
    }

    def from[From: TypeTag] = conversions filter (typeOf[From] <:< _.from)
    def to[To: TypeTag]     = conversions filter (_.to <:< typeOf[To])
    def matching[T <: Function1[_, _] : TypeTag] = {
      val f :: t :: Nil = typeOf[T].typeArguments
      conversions filter (_.matches(f, t))
    }
  }
  // If this is an implicit conversion (either a method/polytype or
  // a value of type Function1) return the from/to types.
  def conversionTypes(tp: Type): Option[(Type, Type)] = tp match {
    case PolyType(_, resultType)                        => conversionTypes(resultType)
    case MethodType(p :: Nil, restpe)                   => Some((p typeSignatureIn tp, restpe))
    case TypeRef(_, Function1Symbol, from :: to :: Nil) => Some((from, to))
    case _                                              => None
  }

  def membersOf[T: TypeTag] : List[ru.Symbol]   = typeOf[T].members.toList
  def declsOf[T: TypeTag] : List[ru.Symbol]     = typeOf[T].declarations.toList
  def implicitsIn[T: TypeTag] : List[ru.Symbol] = membersOf[T] filter (s => s.isTerm && (s hasFlag ru.Flag.IMPLICIT))

  def typesOfMembersOf[T: TypeTag] : List[Type]             = membersOf[T] map (_ typeSignatureIn typeOf[T])
  def typesOfDeclsOf[T: TypeTag] : List[Type]               = declsOf[T] map (_ typeSignatureIn typeOf[T])
  def typesOfImplicitsIn[T: TypeTag] : List[Type]           = implicitsIn[T] map (_ typeSignatureIn typeOf[T])
  def typesOfConversionsIn[T: TypeTag] : List[(Type, Type)] = typesOfImplicitsIn[T] flatMap conversionTypes distinct

  def implicits[Owner: TypeTag] = new ImplicitMatcher[Owner]

  implicit def postfixOps = language.postfixOps // make all postfix ops in this package compile without warning

  private[repl] implicit def javaCharSeqCollectionToScala(xs: JCollection[_ <: CharSequence]): List[String] =
    xs.asScala.toList map ("" + _)

  private def parens(x: Any) = "(" + x + ")"
  private[repl] def tracing[T](msg: String)(res: T): T = res tap (x => repltrace(parens(x)))
  private[repl] def debugging[T](msg: String)(res: T)  = res tap (x => repldbg(parens(x)))

  implicit class ReplInputStreamOps(in: InputStream)(implicit codec: Codec) {
    def bytes(): Array[Byte] = Streamable.bytes(in)
    def slurp(): String      = Streamable.slurp(in)
  }
  implicit class ReplUrlOps(val url: URL) extends AnyVal {
    def slurp(implicit codec: Codec): String = Streamable.slurp(url)
  }
}
