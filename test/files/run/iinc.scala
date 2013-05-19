class A {
  def f() = {
    var sum = 0
    var idx = 0
    while (idx < 100) {
      sum += idx
      idx += 1
    }
    sum
  }

  def g() = {
    var sum: Int = 0
    var idx: Byte = Byte.MinValue
    while (idx < Byte.MaxValue) {
      sum += idx
      idx = (idx + 1).toByte
    }
    sum
  }
}
