/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package settings

trait AdvancedScalaSettings {
  self: AbsScalaSettings =>

  abstract class X extends SettingGroup("-X") {
    val assemextdirs: StringSetting
    val assemname: StringSetting
    val assempath: StringSetting
    val checkinit: BooleanSetting
    val disableassertions: BooleanSetting
    val elidebelow: IntSetting
    val experimental: BooleanSetting
    val future: BooleanSetting
    val generatephasegraph: StringSetting
    val logimplicits: BooleanSetting
    val mainClass: StringSetting
    val migration: BooleanSetting
    val noforwarders: BooleanSetting
    val nojline: BooleanSetting
    val nouescape: BooleanSetting
    val print: PhasesSetting
    val printicode: BooleanSetting
    val printpos: BooleanSetting
    val printtypes: BooleanSetting
    val prompt: BooleanSetting
    val resident: BooleanSetting
    val script: StringSetting
    val showclass: StringSetting
    val showobject: StringSetting
    val showphases: BooleanSetting
    val sourcedir: StringSetting
    val sourcereader: StringSetting
  }
}
