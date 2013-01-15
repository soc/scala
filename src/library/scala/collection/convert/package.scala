/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

package object convert {
  val decorateAsJava: DecorateAsJava                   = new DecorateAsJava { }
  val decorateAsScala: DecorateAsScala                 = new DecorateAsScala { }
  val decorateAll: DecorateAsJava with DecorateAsScala = new DecorateAsJava with DecorateAsScala { }
  val wrapAsJava: WrapAsJava                           = new WrapAsJava { }
  val wrapAsScala: WrapAsScala                         = new WrapAsScala { }
  val wrapAll: WrapAsJava with WrapAsScala             = new WrapAsJava with WrapAsScala { }
}
