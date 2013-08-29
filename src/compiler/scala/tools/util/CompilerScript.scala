package scala.tools
package util

import nsc._
import util.BatchSourceFile

class CompilerScript(settings: Settings) extends (String => Unit) {
  def this() = this(new Settings)
  val global = new Global(settings)

  import global._
  import definitions._

  def init(): this.type = {
    val src = new BatchSourceFile("<init>", "class $script")
    new Run() compileSources List(src);

    this
  }

  def afterTyper[T](op: => T): T = atPhase(currentRun.typerPhase.next)(op)
  def afterUncurry[T](op: => T): T = atPhase(currentRun.uncurryPhase.next)(op)
  def beforeErasure[T](op: => T): T = atPhase(currentRun.erasurePhase)(op)

  object Cmd {
    def apply(line: String): Cmd = {
      val words = (line.trim split "\\s+").toList
      words.head match {
        case "object" | "package" => Members(words.tail mkString " ", true)
        case "class"              => Members(words.tail mkString " ", false)
        case x                    => sys.error("Unknown command: " + x)
      }
    }
  }
  sealed abstract class Cmd {
    def show(): Unit
  }
  case class Members(name: String, isObject: Boolean) extends Cmd {
    // def exclude(sym: Symbol, tpe: Type) = {
    //   if (tpe.isStructuralRefinement)
    //
    //   sym.info
    def exclude(sym: Symbol) = {
      sym.isAnonOrRefinementClass ||
      sym.isAnonymousFunction || {
        (sym.name.toString contains "$anon")
        // if (sym.name.toString contains "$anon") {
        //   println("Why not me? " + sym)
        //   true
        // }
        // else false
      }
    }

    private val badParents: Set[Symbol] = Set(ObjectClass, AnyRefClass, ScalaObjectClass)
    def defStrings(): List[String] = {
      val sym = (
        if (isObject) definitions.getModule(name).moduleClass
        else definitions.getClass(name)
      )
      val pre = sym.info
      def fixParents(tps: List[Type]) = tps filterNot (tp => badParents(tp.typeSymbol)) match {
        case Nil    => List(ObjectClass.tpe)
        case tps    => tps
      }

      afterTyper {
        pre.nonPrivateMembers flatMap { s =>
          val newInfo = (pre memberInfo s) match {
            case RefinedType(parents, decls)         => RefinedType(fixParents(parents), EmptyScope)
            case ClassInfoType(parents, defs, clazz) => ClassInfoType(fixParents(parents), EmptyScope, clazz)
            case TypeRef(pre, sym, args)             => TypeRef(pre, sym, args)
            case tpe                                 => tpe
          }
          s.updateInfo(newInfo)
          if (newInfo == NoType || exclude(s)) None
          else {
            // println("s.info = " + s.info.getClass)
            // println(s.info)
            // None
            Some(s.defString)
          }
        }
      }
    }

    def show() {
      defStrings().sorted foreach println
    }
  }

  def apply(line: String): Unit = apply(Cmd(line))
  def apply(cmd: Cmd): Unit = cmd.show()
}

object CompilerScript {
  def main(args: Array[String]): Unit = {
    val cs = new CompilerScript init();

    args foreach cs
  }
}
