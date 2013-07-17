// package classpath_test

import scala.reflect.io._, classpath._

object Test {
  def timed[T](msg: T => String)(body: => T): T = {
    val start = System.nanoTime
    val res = body
    val elapsed = "%.3f ms".format((System.nanoTime - start) / 1e6)
    println("\n[%8s]  %s\n".format(elapsed, msg(res)))
    res
  }
  class MyLoader(val cp: ClassPath) extends ClassLoader {
    var bytes, success, failed, skipped = 0
    var messages: List[String] = Nil
    def megabytes = "%.2f Mb" format bytes / 1e6
    override def toString = "Loaded " + success + " classes from " + megabytes + " of bytecode, Class creation failed " + failed + " times"

    def make(rep: FileRep): Option[Class[_]] = {
      val bytes = cp findBytes rep.path
      try Some(defineClass(rep.className, bytes, 0, bytes.length)) catch { case t: Throwable =>
        messages ::= "%-80s  %s".format(rep.className, t)
        None
      }
    }
    def makeAll(reps: Seq[FileRep]): this.type = {
      reps foreach { rep =>
        val arr = cp findBytes rep.path
        make(rep) match {
          case Some(clazz) =>
            bytes += arr.length
            success += 1
            if (success % 25 == 0)
              Console.err.print(".")
          case _ =>
            failed += 1
            if (failed % 25 == 0)
              Console.err.print("!")
        }
        if (((success + failed) % 2000) == 0)
          Console.err.println("")
      }
      this
    }
  }

  def main(args: Array[String]): Unit = {
    val cp = timed[ClassPath](_.summary)(
      if (args.isEmpty) ClassPath.default
      else ClassPath(args(0))
    )
    val loader = new MyLoader(cp)
    timed((x: MyLoader) => x.toString) {
      loader makeAll cp.reps.filter(_.isClass)
    }
  }
}
