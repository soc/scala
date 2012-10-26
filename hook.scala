def sourceAnnotation[T <: Global](global: T) = {
  val ch = new CompilerHooks[T](global)
  import ch.global._
  import definitions._
  val sourceCodeClass = getRequiredClass[sourceCode]

  def skipDefn(t: Tree) = t match {
    case defn: ValOrDefDef =>
      (    (defn eq emptyValDef)
        || (defn.mods hasFlag PARAM | PARAMACCESSOR | SYNTHETIC)
        || (defn.rhs eq EmptyTree)
        || (defn.pos.sourceCode == "")
      )
    case _ => false
  }

  ch parsed {
    case defn: ValOrDefDef if !skipDefn(defn) =>
      atPos(defn.pos) {
        val annot = gen.mkAnnotation(sourceCodeClass)("source" -> Literal(Constant(defn.pos.sourceCode)))
        defn match {
          case defn: ValDef => copyValDef(defn)(mods = defn.mods withAnnotations List(annot))
          case defn: DefDef => copyDefDef(defn)(mods = defn.mods withAnnotations List(annot))
        }
      }
  }
}
