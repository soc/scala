package scala

package object reflect extends reflect_compat {

  lazy val basis: base.Universe = new base.Base

  def classTag[T](implicit ctag: ClassTag[T]) = ctag
  // typeTag incantation is defined inside scala.reflect.basis and scala.reflect.runtime.universe

  // ClassTag class is defined in ClassTag.scala
  type TypeTag[T]          = scala.reflect.basis.TypeTag[T]

  // ClassTag object is defined in ClassTag.scala
  lazy val TypeTag         = scala.reflect.basis.TypeTag
}
