/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.repl

import scala.sys._
import Prop._

class ReplProps {
  private def bool(name: String) = BooleanProp.keyExists(name)
  private def int(name: String) = IntProp(name)

  val jlineDebug = bool("jline.internal.Log.debug")
  val jlineTrace = bool("jline.internal.Log.trace")

  val info  = bool("scala.repl.info")
  val debug = bool("scala.repl.debug")
  val trace = bool("scala.repl.trace")
  val power = bool("scala.repl.power")

  val replInitCode    = Prop[JFile]("scala.repl.initcode")
  val replAutorunCode = Prop[JFile]("scala.repl.autoruncode")
  val powerBanner     = Prop[JFile]("scala.repl.power.banner")

  val vids = bool("scala.repl.vids")
  val maxPrintString = int("scala.repl.maxprintstring")
}
