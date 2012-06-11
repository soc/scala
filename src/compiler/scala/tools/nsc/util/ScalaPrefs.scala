/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package util

import io.Sources

trait ScalaPrefs {
  def codeSources: Sources
  def exceptionFormatter: Exceptional.Formatter
}

object ScalaPrefs {
  @annotation.implicitWeight(-1)
  implicit object DefaultScalaPrefs extends ScalaPrefs {
    def codeSources        = Sources.defaultSources
    def exceptionFormatter = Exceptional.Formatter(this)
  }

  def apply(implicit prefs: ScalaPrefs): ScalaPrefs = prefs
}
