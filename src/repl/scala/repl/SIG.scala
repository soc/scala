/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.repl

import sun.misc.{ Signal, SignalHandler }
import sun.misc.SignalHandler.{ SIG_DFL, SIG_IGN }

class Handler(handler: PartialFunction[Signal, Unit]) extends SignalHandler {
  def handle(sig: Signal) = if (handler.isDefinedAt(sig)) handler(sig)
}

class WSignal(val name: String) {
  val sig                   = new Signal(name)
  def number                = sig.getNumber
  def raise()               = Signal raise sig
  def handle(body: => Unit) = Signal.handle(sig, new Handler({ case `sig` => body }))

  override def equals(other: Any) = other match {
    case x: WSignal => name == x.name
    case _          => false
  }
  override def hashCode = name.##
  override def toString = "SIG" + name
}

object SIG {
  private implicit def mkWSignal(name: String): WSignal = new WSignal(name)
  private lazy val signalNumberMap = all map (x => x.number -> x) toMap

  def all = List(
    HUP, INT, QUIT, ILL, TRAP, ABRT, EMT, FPE,    // 1-8
    KILL, BUS, SEGV, SYS, PIPE, ALRM, TERM, URG,  // 9-15
    STOP, TSTP, CONT, CHLD, TTIN, TTOU, IO, XCPU, // 16-23
    XFSZ, VTALRM, PROF, WINCH, INFO, USR1, USR2   // 24-31
  )
  /** Signals which are either inaccessible or which seem like
   *  particularly bad choices when looking for an open one.
   */
  def reserved   = Set(QUIT, TRAP, ABRT, KILL, BUS, SEGV, ALRM, STOP, INT)
  def unreserved = all filterNot reserved

  def dump() = all foreach (x => println("%2s %s".format(x.number, x)))

  def apply(sigNumber: Int): WSignal = signalNumberMap(sigNumber)
  def apply(name: String): WSignal   = all find (_.name == name) getOrElse new WSignal(name)

  def HUP: WSignal    = "HUP"
  def INT: WSignal    = "INT"
  def QUIT: WSignal   = "QUIT"
  def ILL: WSignal    = "ILL"
  def TRAP: WSignal   = "TRAP"
  def ABRT: WSignal   = "ABRT"
  def EMT: WSignal    = "EMT"
  def FPE: WSignal    = "FPE"
  def KILL: WSignal   = "KILL"
  def BUS: WSignal    = "BUS"
  def SEGV: WSignal   = "SEGV"
  def SYS: WSignal    = "SYS"
  def PIPE: WSignal   = "PIPE"
  def ALRM: WSignal   = "ALRM"
  def TERM: WSignal   = "TERM"
  def URG: WSignal    = "URG"
  def STOP: WSignal   = "STOP"
  def TSTP: WSignal   = "TSTP"
  def CONT: WSignal   = "CONT"
  def CHLD: WSignal   = "CHLD"
  def TTIN: WSignal   = "TTIN"
  def TTOU: WSignal   = "TTOU"
  def IO: WSignal     = "IO"
  def XCPU: WSignal   = "XCPU"
  def XFSZ: WSignal   = "XFSZ"
  def VTALRM: WSignal = "VTALRM"
  def PROF: WSignal   = "PROF"
  def WINCH: WSignal  = "WINCH"
  def INFO: WSignal   = "INFO"
  def USR1: WSignal   = "USR1"
  def USR2: WSignal   = "USR2"
}
