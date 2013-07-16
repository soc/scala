package scala.tools.nsc
package util

import java.net.URL

trait SimpleClassPath {
  def isEmpty = asClasspathString == ""
  def name: String
  def origin: Option[String]
  def asURLs: List[URL]
  def asClasspathString: String
  def context: ClassPath.ClassPathContext
  def entries: Seq[SimpleClassPath]
  def classes: Seq[ClassRep]
  def packages: Seq[SimpleClassPath]
  def sourcepaths: Seq[AbstractFile]
  def findClass(name: String): ClassRep
  def findSourceFile(name: String): AbstractFile
  def sortString: String
  def show(): Unit
}
object SimpleClassPath {
  def apply(s: String): SimpleClassPath = ???
}
