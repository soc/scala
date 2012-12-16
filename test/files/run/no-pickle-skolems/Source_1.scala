package s

trait Foo { def quux[CC[X]](implicit bippyFoo: CC[Int]): Unit }

class Bar extends Foo { def quux[CC[X]](implicit bippyBar: CC[Int]): Unit = ??? }
