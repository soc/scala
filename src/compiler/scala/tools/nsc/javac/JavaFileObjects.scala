/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

// package scala.tools.nsc
package javac

import scala.tools.nsc.io.{ AbstractFile, Directory, VirtualDirectory }
import scala.collection.{ mutable, immutable }
import javax.tools._
import java.io._
import JavaFileObject.Kind
import java.net.URI

//
// trait JavaCompilerInRepl {
//   private lazy val javaCompiler = new JavaCompiler(classLoader, virtualDirectory)
//   private def javaImports() = {
//     javaCompiler.compiledFiles map ("import " + _ + "\n") mkString
//   }
//
//   def compileJavaSnippet(code: String) =
//     javaCompiler.compileSnippet(code)
//
//   def compileJavaUnit(code: String): Boolean =
//     javaCompiler.compileUnit(code)
//
//   def loadJavaUnit(name: String, code: String) = {
//     javaCompiler.compileUnit(code)
//     loadByName(JavaCompiler.compilerPackage + "." + name)
//   }
// }

trait JavaFileObjects {
  abstract class WrapJavaFileObject extends JavaFileObject {
    protected def cleanup(str: String) = str.replace('.', File.separatorChar) stripSuffix "/"
    def isSourceFile: Boolean
    def ext = getKind.extension

    def delete()                                          = false
    def getAccessLevel()                                  = null
    def getKind()                                         = if (isSourceFile) Kind.SOURCE else Kind.CLASS
    def getLastModified()                                 = 0
    def getName()                                         = toUri.getPath
    def getNestingKind()                                  = null
    // def getCharContent(ignoreEncodingErrors: Boolean): String
    // def isNameCompatible(simpleName: String, kind: Kind): Boolean
    def openInputStream(): InputStream                    = throw new UnsupportedOperationException
    def openOutputStream(): OutputStream                  = throw new UnsupportedOperationException
    def openReader(ignoreEncodingErrors: Boolean): Reader = throw new UnsupportedOperationException
    def openWriter(): Writer                              = throw new UnsupportedOperationException
    // def toUri(): URI                                   = URI.create("string:///" + cleanup(name) + SOURCE.extension)
  }

  case class StringFileObject(name: String, code: String) extends WrapJavaFileObject {
    def isSourceFile = true
    def getCharContent(ignoreEncodingErrors: Boolean)    = code
    def isNameCompatible(simpleName: String, kind: Kind) = true
    def toUri(): URI                                     = URI.create("string:///" + name.replace('.', '/') + Kind.SOURCE.extension)

    override def openReader(ignoreEncodingErrors: Boolean) = new StringReader(code)
    override def openWriter()                              = throw new IllegalStateException
    override def toString()                                = "\"" + code + "\""
  }

  class AbstractFileFileObject(file: AbstractFile, kind: Kind) extends JavaFileObject {
    def this(file: AbstractFile) = this(file, Kind.SOURCE)

    def delete()                                         = try { file.delete ; true } catch { case _ => false }
    def getAccessLevel()                                 = null
    def getCharContent(ignoreEncodingErrors: Boolean)    = file.toCharArray
    def getKind()                                        = kind
    def getLastModified()                                = file.lastModified
    def getName()                                        = file.name
    def getNestingKind()                                 = null
    def isNameCompatible(simpleName: String, kind: Kind) = {
      println("isNameCompatible(%s, %s)".format(simpleName, kind))
      println("... file.path = " + file.path + " toUri = " + toUri)
      (file.name contains simpleName) || (simpleName contains file.name)
    }
    def openInputStream()                                = file.input
    def openOutputStream()                               = file.output
    def openReader(ignoreEncodingErrors: Boolean)        = new InputStreamReader(openInputStream())
    def openWriter()                                     = new OutputStreamWriter(openOutputStream())
    def toUri(): URI                                     = file.file.toURI

    override def toString() = file.toString
  }

  class VirtualFileManager(
    val output: VirtualDirectory,
    val loader: ClassLoader,
    underlying: JavaFileManager
  ) extends ForwardingJavaFileManager[JavaFileManager](underlying) {

    val classFiles = mutable.HashSet[String]()

    def clean(s: String) = s.replace('.', '/') stripSuffix "/"
    def canonFind(path: String) = {
      val xs               = path split '/' filterNot (_ == "") toList;
      val (dirs, filename) = (xs.init, xs.last)
      val subdir           = dirs.foldLeft(output: AbstractFile)(_ subdirectoryNamed _)
      val file             = subdir.fileNamed(filename)

      logging(new AbstractFileFileObject(file))
    }

    def log(args: Any*) = println(args.mkString(", "))
    def logging[T](x: T): T = {
      log("Noted: " + x)
      x
    }

    override def getClassLoader(location: JavaFileManager.Location): ClassLoader = {
      log('getClassLoader, location)
      loader
    }

    override def getFileForInput(location: JavaFileManager.Location, packageName: String, relativeName: String): FileObject = {
      log('getFileForInput, location, packageName, relativeName)
      canonFind(clean(packageName) + "/" + relativeName)
    }
    override def getFileForOutput(location: JavaFileManager.Location, packageName: String, relativeName: String, sibling: FileObject): FileObject = {
      log('getFileForOutput, location, packageName, relativeName, sibling)
      canonFind(clean(packageName) + "/" + relativeName)
    }

    override def getJavaFileForInput(
      location: JavaFileManager.Location, className: String, kind: JavaFileObject.Kind): JavaFileObject =
    {
      log('getJavaFileForInput, location, className, kind)
      canonFind(clean(className) + kind.extension)
    }
    override def getJavaFileForOutput(
      location: JavaFileManager.Location, className: String, kind: JavaFileObject.Kind, sibling: FileObject): JavaFileObject =
    {
      log('getJavaFileForOutput, location, className, kind, sibling)
      if (kind == Kind.CLASS)
        classFiles += className

      canonFind(clean(className) + kind.extension)
    }
  }
}

object JavaFileObjects extends JavaFileObjects { }