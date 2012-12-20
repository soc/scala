/** Determine the return type of a X.getClass() expression.
 */
def getClassExpressionType(owner: Symbol, tp: Type): Type = {
  if (phase.erasedTypes) ClassClass.tpe
  else if (isScalaValueType(tp)) ClassType(tp.widen)
  else if (isPhantomClass(tp.typeSymbol)) boundedClassType(AnyClass.tpe)
  else boundedClassType(widenEnclosedClasses(owner, tp.widen))
}

// A type function from T => Class[U], used to determine the return
// type of getClass calls.  The returned type is:
//
//  1. If T is a value type, Class[T].
//  2. If T is a phantom type (Any or AnyVal), Class[_].
//  3. If T is a local class, Class[_ <: |T|].
//  4. Otherwise, Class[_ <: T].
//
// Note: AnyVal cannot be Class[_ <: AnyVal] because if the static type of the
// receiver is AnyVal, it implies the receiver is boxed, so the correct
// class object is that of java.lang.Integer, not Int.
//
// TODO: If T is final, return type could be Class[T].  Should it?

/** Determine the return type of a classOf[X] expression.
 *  It is a constant type unless it references a locally
 *  defined class.
 */
def classOfExpressionType(owner: Symbol, tp: Type): Type = {
  val newtp = widenEnclosedClasses(owner, tp)
  if (tp eq newtp) ConstantType(Constant(tp))
  else boundedClassType(newtp)
  // else ConstantType(Constant(newtp))
}

/** Given type U, creates a Type representing Class[_ <: U].
 */
def boundedClassType(upperBound: Type) = {
  val eparams = typeParamsToExistentials(ClassClass, ClassClass.typeParams)
  existentialAbstraction(
    eparams,
    ClassType(eparams.head setInfo TypeBounds.upper(dropRefinements(upperBound)) tpe)
  )
}
/** To avoid unchecked warnings on polymorphic classes, translate
 *  a Foo[T] into a Foo[_] for use in the pattern matcher.
 */
def typeCaseType(clazz: Symbol) = clazz.tpe.normalize match {
  case TypeRef(_, sym, args) if args.nonEmpty => newExistentialType(sym.typeParams, clazz.tpe)
  case tp                                     => tp
}
