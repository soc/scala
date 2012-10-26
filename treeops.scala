class TreeOps(tree: Tree) extends super.TreeOps(tree) {
  override def posString = {
    if (tree.pos.isDefined) "(at %s:%s)".format(tree.pos.sourceName, tree.pos.safeLine)
    else ""
  }
  def inBraces(label: String)(body: => Unit) {
    println(label + " \u007b\n")
    body
    println("\u007d")
  }
  def dump() {
    def show(name: String, thing: Any) {
      println("  %-12s %s".format(name, thing))
    }
    inBraces(tree.summaryString) {
      show("symbol", tree.symbol)
      if (tree.symbol ne null)
        show("flags", tree.symbol.hasFlagsToString(-1L))
      show("tpe", tree.tpe)
      show("pos", tree.pos)
      show("code \u007b", "\n" + tree.pos.sourceCode + "\n  \u007d")

      tree match {
        case t: MemberDef =>
          show("modifiers", t.mods.hasFlagsToString(-1L))
          show("annotations", t.mods.annotations.mkString(", "))
        case t: TypeTree if t.original != null =>
          show("original", t.original.summaryString)
        case _ =>
      }
      show("children \u007b", tree.children.map(_.summaryString).mkString("\n    ", "\n    ", "\n  \u007d"))
    }
  }
  def dumpMemberDefs(tree: Tree) {
    tree foreach {
      case t: MemberDef if !t.mods.isParameter => t.dump()
      case _                                   =>
    }
  }
}
