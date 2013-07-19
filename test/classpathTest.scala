import scala.reflect.io._
import classpath._
import classpath.Debug._

object Test {
  class AmbitiousLoader(val cp: ClassPath) extends ClassLoader {
    var bytes, success, failed, skipped = 0
    var messages: List[String] = Nil
    def megabytes = "%.2f Mb" format bytes / 1e6
    override def toString = "Loaded " + success + " classes from " + megabytes + " of bytecode, " + failed + " failed to load."

    def make(rep: PathRep): Option[Class[_]] = {
      val bytes = cp findBytes rep
      try Some(defineClass(rep.className, bytes, 0, bytes.length)) catch { case t: Throwable =>
        messages ::= "%-80s  %s".format(rep.className, t)
        None
      }
    }
    def makeAll(reps: Seq[PathRep]): this.type = {
      reps foreach { rep =>
        val arr = cp findBytes rep
        make(rep) match {
          case Some(clazz) =>
            bytes += arr.length
            success += 1
            if (success % 25 == 0)
              pokeErr(".")
          case _ =>
            failed += 1
            if (failed % 25 == 0)
              pokeErr("!")
        }
        if (((success + failed) % 2000) == 0)
          printErr("")
      }
      this
    }
  }

  def main(args: Array[String]): Unit = {
    val cp = Debug.timed[ClassPath](_.summary)(
      try if (args.isEmpty) ClassPath() else ClassPath(app = args(0))
      finally printErr("-------------")
    )
    println("\nInitiated class loading bonanza:")
    val loader = new AmbitiousLoader(cp)
    Debug.timed((x: AmbitiousLoader) => x.toString) {
      try loader makeAll cp.paths.filter(_.isClass)
      finally println("")
    }
    println("")
  }
}

Test main args