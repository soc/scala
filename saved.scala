
class ExistentialTypeMap extends TypeMap {
  private var expanded  = immutable.Set[Symbol]()
  private var generated = immutable.Set[Type]()

  def cycleType: Type = AnyClass.tpe
  def shouldExpand(sym: Symbol, args: List[Type]) = args.isEmpty

  def apply(tp: Type): Type = tp match {
    case TypeRef(pre, sym, args) if shouldExpand(sym, args) =>
      if (expanded(sym)) cycleType
      else try {
        expanded += sym
        val eparams = mapOver(typeParamsToExistentials(sym))
        existentialAbstraction(eparams, typeRef(apply(pre), sym, eparams map (_.tpe)))
      }
      finally expanded -= sym

    case ExistentialType(_, _) if !generated(tp) => // to avoid infinite expansions. todo: not sure whether this is needed
      val result = mapOver(tp)
      generated += result
      result
    case _ =>
      mapOver(tp)
  }
}

def rawToExistential = new ExistentialTypeMap {
  override def cycleType: Type = AnyRefClass.tpe
  override def shouldExpand(sym: Symbol, args: List[Type]) = (
       args.isEmpty
    && sym.isClass
    && sym.typeParams.nonEmpty
    && sym.isJavaDefined
  )
}


def typeParametersInType(tpe: Type): List[Symbol] = {
  val syms = ListBuffer[Symbol]()
  val seen = mutable.Set[Symbol]()

  def loop(tp: Type) {
    val sym = tp.typeSymbol
    if (!seen(sym)) {
      seen += sym
      if (sym.isTypeParameter)
        syms += sym
      if (sym.isAbstractType)
        loop(sym.info)
    }
  }
  tpe foreach loop
  syms.toList.distinct
}

/** Like classExistentialType, but takes a partial function which can
 *  contribute additional types to each parameter's upper bound.
 */
def deriveExistentialType(clazz: Symbol)(uppers: PartialFunction[Symbol, Type]): Type = {
  classExistentialType(clazz) match {
    case et @ ExistentialType(eparams, underlying) =>
      val eparams1 = eparams mapConserve (ep =>
        if (uppers isDefinedAt ep) {
          val TypeBounds(lo, hi) = ep.info.bounds
          ep.cloneSymbol setInfo TypeBounds(lo, intersectionType(List(hi, uppers(ep))))
        }
        else ep
      )
      if (eparams1 eq eparams) et
      else ExistentialType(eparams1, underlying)
    case tp => tp
  }
}


/** Trims duplicate parents and Any out of a list of parents.
 *  TODO - would like to be more aggressive (Object, ScalaObject,
 *  use spanningTypes, etc.) but not sure if this has an
 *  undesirable impact somewhere.
 */
def elimRedundantParents(parents: List[Type]): List[Type] = {
  def loop(ps: List[Type]): List[Type] = ps match {
    case Nil                                     => Nil
    case p :: rest if (p.typeSymbol eq AnyClass) => loop(rest)
    case p :: rest                               => p :: loop(rest filterNot (_ =:= p))
  }
  loop(parents)
}
