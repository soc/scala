package scala.repl
package api

import scala.tools.nsc._
import io.{ VirtualDirectory }

trait Repl {
  def settings: Settings
  def virtualDirectory: VirtualDirectory
}