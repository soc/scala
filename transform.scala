
class Transformer extends super.Transformer {
  private var failStack: List[Tree] = Nil
  private def mkFailStack = failStack map treeTrace mkString "\n"

  def codeWidth   = 36
  def symbolWidth = 20
  def treeWidth   = 60

  private def treeTrace(tree: Tree) = {
    val (l, c) = (
      if (tree.pos.isDefined) tree.pos lineWithCarat codeWidth
      else (("" + tree, "?"))
    )
    (   "\n  "
      + ljust(treeWidth)(tree.summaryString)
      + " ( " + ljust(codeWidth)(l) + " )"
      + indentNextLine(treeWidth + 3)
      + c
    )
  }

  override def transform(tree: Tree): Tree = {
    try super.transform(tree) catch {
      case t @ (_: FatalError | _: AssertionError) =>
        failStack ::= tree
        throw t
    }
  }

  def transformUnit(unit: CompilationUnit) {
    def failMsg =
      try mkFailStack + supplementErrorMessage("Fatal error while transforming "+unit)
      finally failStack = Nil

    try unit.body = transform(unit.body) catch {
      case t: FatalError     => throw new FatalError(failMsg + t.msg)
      case t: AssertionError => throw new AssertionError(failMsg + t.getMessage, t.getCause)
    }
  }
}

