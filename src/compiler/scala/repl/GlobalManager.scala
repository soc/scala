package scala.repl

import scala.tools.nsc.Global

abstract class GlobalManager[T <: Global with Singleton](val global: T) {
  manager =>

  trait SameGlobal { val global: manager.global.type = manager.global }
}

trait ShowGlobal[T <: Global with Singleton] { }
