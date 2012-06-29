import java.lang.instrument._
import scala.tools.nsc.{ settings => _, _ }
import interpreter.RedefiningClassLoader
import io._

:power

val cl = repl.classLoader
// val cl = new RedefiningClassLoader
val inst = scala.tools.util.JVMAgent.inst
val c1 = File("./work/c1/C.class").bytes().toArray
val c2 = File("./work/c2/C.class").bytes().toArray
type CType = { def foo: String }

def shazam[T](name: String, f: T => Unit): Unit = {
  val clazz: Class[_] = cl.tryToInitializeClass(name).get
  val instance        = clazz.newInstance().asInstanceOf[T]
  f(instance)
}

def shazamOn(bytes: Array[Byte]) = {
  cl.recordClass("C", bytes)
  shazam[CType]("C", x => println(x.foo))
  cl.getBytesForClass("C")
}

val bs1 = shazamOn(c1)
val bs2 = shazamOn(c2)

(bs1 == c1)
(bs2 == c2)
