package scala.reflect

import scala.runtime.ScalaRunTime._
import mirror._
import definitions._

object TagInterop {
  def arrayTagToClassManifest[T](tag: ArrayTag[T]): ClassManifest[T] = {
    val erasure = arrayElementClass(tag)
    if (erasure.isArray) {
      val elementClass = arrayElementClass(erasure)
      val elementManifest = arrayTagToClassManifest(ClassTag(elementClass))
      ClassManifest.arrayType(elementManifest).asInstanceOf[ClassManifest[T]]
    } else {
      ClassManifest.fromClass(erasure.asInstanceOf[Class[T]])
    }
  }
}