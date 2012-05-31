  implicit def treeOps(tree: Tree): TreeOps = new TreeOps(tree)

  class CompressSelectTransformer extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case Select(qual, name) =>
        transform(qual) match {
          case Ident(pre) => Ident(name prepend pre + ".")
          case _          => tree
        }
      case t => super.transform(t)
    }
  }

  class ForeachWithDepthTraverser(f: (Int, Tree) => Unit) extends Traverser {
    private var currentDepth = 0
    private def atDepth(depth: Int)(body: => Unit) {
      val saved = currentDepth
      currentDepth = depth
      try body
      finally currentDepth = saved
    }

    def shouldPrune(t: Tree) = t match {
      case Import(_, _) => true
      case _            => false
    }

    override def traverseTrees(trees: List[Tree]) = {
      val current = currentDepth
      trees foreach (t => atDepth(current)(traverse(t)))
    }

    override def traverse(t: Tree) {
      f(currentDepth, t)
      if (shouldPrune(t)) () else {
        currentDepth += 1
        try super.traverse(t)
        finally currentDepth -= 1
      }
    }
  }

  class TreeOps(tree: Tree) {
    def dumpColorized() {
      var line = 0
      var lastIndent = 0
      def drawBackTo(level: Int) = {
        lastIndent to level by -1 foreach (d =>
          println("| " * (d - 1) + """\-""")
        )
      }
      import scala.tools.util.color._
      object colors {
        def apply(t: Tree): CString      = t.shortClass in Yellow
        def apply(m: MemberDef): CString = this(m: Tree) + " " + this(m.name)
        def apply(n: Name): CString      = n.decode in ( if (n.isTypeName) Cyan.bright else Red.bright )
        def apply(id: Ident): CString    = apply(id.name)
        def keyword(s: String): CString  = s in Yellow.bright
        def apply(l: Literal): CString   = ("" + l) in Green.bright
      }
      def selector_s(sel: ImportSelector) = {
        val ImportSelector(name, _, rename, _) = sel
        if (name == rename) name.toString in White
        else (name + " => " in White) + (rename.toString in Red)
      }
      def selectors_s(sels: List[ImportSelector]) = {
        val xs = sels map selector_s
        if (xs.size == 1 && !xs.head.toString.contains(" ")) "" + xs.head
        else xs.mkString("{ ", ", ", " }")
      }

      def colorized(t: Tree): CString = t match {
        case null         => "<null>" in Red.bright
        case x: TypeTree  => "TypeTree" in Yellow
        case x: MemberDef => colors(x)
        case x: Import    =>
          ( colors.keyword("import") + " "
              + colors(newTermName("" + x.expr))
              + "." + selectors_s(x.selectors)
          )
        case x: Ident     => colors(x)
        case x: Literal   => colors(x)
        case x            => t.shortClass.stripSuffix("Tree") in Yellow
      }

      tree.compressSelects foreachWithDepth { (depth, t) =>
        line += 1
        drawBackTo(depth + 1)
        lastIndent = depth
        println("| " * depth + colorized(t))
      }
      drawBackTo(1)
    }
    def foreachWithDepth(f: (Int, Tree) => Unit) {
      new ForeachWithDepthTraverser(f) traverse tree
    }
    def compressSelects: Tree = {
      new CompressSelectTransformer transform tree
    }
  }