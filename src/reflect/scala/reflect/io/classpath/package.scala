package scala
package reflect
package io

package object classpath extends PackageDebug {
  val NoBytes: Bytes = Array.emptyByteArray
  type Bytes         = Array[Byte]
  type ZipFile       = java.util.zip.ZipFile
  type jFile         = java.io.File
  type tailrec       = scala.annotation.tailrec
}
