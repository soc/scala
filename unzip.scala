import scala.tools.nsc.util.ClassPath._
import scala.tools.nsc.io._
import scala.sys.process._

object Test {
  def main(args: Array[String]): Unit = {
    ( args.toList flatMap (cp => expandPath(cp, expandStar = true))
        map (f => File(f))
        filter (f => f.canRead && f.hasExtension("jar"))
        foreach (jar =>
          Process(s"/usr/bin/unzip -l $jar").lines
            drop 3
            takeWhile (s => !(s startsWith " ----"))
            foreach (s => println(s split ' ' last))
        )
    )
  }
}
