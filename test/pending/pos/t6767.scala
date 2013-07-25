trait Foo {
  def andThis(): this.type = this

  def setFoo1()            = andThis()
  def setFoo2(): this.type = andThis()
}

class Bar extends Foo {
  def f1(): Bar = setFoo1() // does not compile
  def f2(): Bar = setFoo2() // compiles
}
