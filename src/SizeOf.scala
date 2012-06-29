import scala.util.control.Exception.ignoring

class ObjectSizer(clazz: Class[_]) {
  def SAMPLE_SIZE    = 10000
  def SLEEP_INTERVAL = 100

  def calculate(sample: Int = SAMPLE_SIZE): Int = {
    val constructor = clazz.getConstructor()
    val arr         = new Array[AnyRef](sample)
    var i           = 0
    val start       = getMemoryUse

    while (i < sample) {
      arr(i) = constructor.newInstance().asInstanceOf[AnyRef]
      i += 1
    }
    ((getMemoryUse - start) / sample) toInt
  }

  private def allMem = Runtime.getRuntime().totalMemory()
  private def freeMem = Runtime.getRuntime().freeMemory()

  private def getMemoryUse(): Long = {
    putOutTheGarbage()
    val totalMemory = allMem
    putOutTheGarbage()
    (totalMemory - freeMem)
  }
  private def putOutTheGarbage() { collectGarbage() ; collectGarbage() }
  private def collectGarbage() {
    System.gc();
    Thread sleep SLEEP_INTERVAL
    System.runFinalization()
    Thread sleep SLEEP_INTERVAL
  }
}

object ObjectSizer {
  def apply(clazz: Class[_]) = new ObjectSizer(clazz)
  def sizeOf(clazz: Class[_]) = apply(clazz).calculate()

  def main(args: Array[String]): Unit = {

  }
}
