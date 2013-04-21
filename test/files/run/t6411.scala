class X[T](val i: T) extends AnyVal

object a {
  def y[T](x: X[T]) = x.i
}

object Test {
  def main(args: Array[String]) {
    val cm      = scala.reflect.runtime.currentMirror
    val moduleA = cm.reflect(a)
    val methodY = moduleA.symbol.typeSignature.declarations.last.asMethod
    val m       = moduleA.reflectMethod(methodY)

    m(new X(0))
    // java.lang.NoClassDefFoundError: no Java class corresponding to ErasedValueType(X[T]) found
    //   at scala.reflect.runtime.JavaMirrors$JavaMirror.typeToJavaClass(JavaMirrors.scala:1225)
    //   at scala.reflect.runtime.JavaMirrors$JavaMirror$$anonfun$methodToJava$1$$anonfun$27.apply(JavaMirrors.scala:1192)
    //   at scala.reflect.runtime.JavaMirrors$JavaMirror$$anonfun$methodToJava$1$$anonfun$27.apply(JavaMirrors.scala:1192)
  }
}
