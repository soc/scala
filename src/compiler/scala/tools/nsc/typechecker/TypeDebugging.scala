/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package typechecker

trait TypeDebugging {
  self: Global =>

  import definitions._

  /** Switch to turn on detailed type logs */
  var printTypings = false
  var printInfers = false

  object typeDebug {
    import scala.Console._

    // To enable decent error messages when the typer crashes.
    // TODO - this only catches trees which go through def typed,
    // but there are all kinds of back ways - typedClassDef, etc. etc.
    // Funnel everything through one doorway.
    var lastTreeToTyper: Tree = EmptyTree

    // Put an empty tree on it so we can always check head without concern
    private var printTypingStack: List[Tree] = EmptyTree :: Nil

    // TODO - account for colors so the color of a multiline string
    // doesn't infect the connector lines
    private def typingIndent = "|    " * (printTypingStack.length - 1)

    @inline def printTyping(s: => String) = {
      if (printTypings && !noPrintTyping(printTypingStack.head)) {
        // This allows for call sites of printTyping to have a "filter"
        // which is only run when typing information is being printed.
        // That is, we want to be able to write code like
        //   printTyping({ val info = foo.toString ; if (!info.isInteresting) "" else info })
        // If we have to do the isInteresting test before calling printTyping,
        // it defeats the purpose of the by-name argument.
        val s1 = s.replaceAll("\n", "\n" + typingIndent)
        if (s1 != "")
          println(typingIndent + s1)
      }
    }
    @inline def printInference(s: => String) = {
      if (printInfers)
        println(s)
    }

    def printImplicitSearchOf[T](tree: Tree)(op: => T): T = {
      lazy val result = op

      try {
        printTyping("""|-- implicit search: $tree ${context.undetparamsString}""")
        printTypingStack ::= tree
        result
      }
      finally {
        printTypingStack = printTypingStack.tail
        printTyping("""\-> """ + ", implicit search yielded: " + result)
      }
    }

    def printTypingOf(tree: Tree, mode: Int, pt: Type, context: analyzer.Context)(op: => Tree): Tree = {
      lazy val typedTree = op
      val alreadyTyped = tree.tpe ne null

      def before(): String = {
        def implicits_s = (
          if (context.enrichmentEnabled)
            if (context.implicitsEnabled) ""
            else "implicits: " + inRed("enrichment")
          else "implicits: " + inRed("disabled")
        )
        def owner_long_s = (
          if (settings.debug.value) {
            def flags_s = context.owner.debugFlagString match {
              case "" => ""
              case s  => " with flags " + s
            }
            s", a ${context.owner.shortSymbolClass}$flags_s"
          }
          else ""
        )
        def owner_s = "" + context.owner + (
          if (context.owner.isClass) ""
          else " in " + context.owner.enclClass
        )
        def tree_s = inGreen(ptTree(tree))
        def pt_s = if (pt.isWildcard) "" else s": pt=$pt"
        def what = if (alreadyTyped) "(already typed) " else ""

        "%s%s%s (%s)".format(what, tree_s, pt_s, ptLine(
          "undetparams"       -> context.undetparams,
          ""         -> implicits_s,
          ""                  -> modeString(mode),
          ""                  -> ( if (context.bufferErrors) inRed("silent") else "" ),
          "owner"             -> (owner_s + owner_long_s))
        )
      }
      def after(): String = {
        if (typedTree eq null)
          "[null]"
        else tree match {
          case md: MemberDef if typedTree.tpe eq NoType => inLightGreen(s"[${md.keyword} ${md.name}]") + "\n"
          case _                                        => inBlue(typedTree.tpe.toLongString)
        }
      }

      try {
        printTyping("""|-- """ + before)
        printTypingStack ::= tree
        typedTree
      }
      finally {
        printTypingStack = printTypingStack.tail
        printTyping("""\-> """ + after)
      }
    }

    // Some trees which are typed with mind-numbing frequency and
    // which add nothing by being printed. Did () type to Unit? Let's
    // gamble on yes.
    def noPrintTyping(t: Tree): Boolean = t match {
      case PackageDef(_, _)                                               => false
      case TypeBoundsTree(lo, hi)                                         => noPrintTyping(lo) && noPrintTyping(hi)
      case Select(sel, nme.scala_)                                        => noPrintTyping(sel)
      case Select(sel, tpnme.Nothing | tpnme.Any | tpnme.AnyRef)          => noPrintTyping(sel)
      case Block(Nil, expr)                                               => noPrintTyping(expr)
      case Apply(fn, Nil)                                                 => noPrintTyping(fn)
      case Block(stmt :: Nil, expr)                                       => noPrintTyping(stmt) && noPrintTyping(expr)
      case DefDef(_, nme.CONSTRUCTOR, Nil, ListOfNil, _, rhs)             => noPrintTyping(rhs)
      case Literal(Constant(()))                                          => true
      case Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR) => true
      case Ident(nme.ROOTPKG)                                             => true
      case _                                                              => false
    }

    private val colorsOk = sys.props contains "scala.color"
    private def inColor(s: String, color: String) = if (colorsOk && s != "") color +        s + RESET else s
    private def inBold(s: String, color: String)  = if (colorsOk && s != "") color + BOLD + s + RESET else s

    def inLightGreen(s: String)    = inColor(s, GREEN)
    def inGreen(s: String): String = inBold(s, GREEN)
    def inRed(s: String): String   = inBold(s, RED)
    def inBlue(s: String): String  = inBold(s, BLUE)
    def inCyan(s: String): String  = inBold(s, CYAN)

    private def to_s(x: Any): String = x match {
      // otherwise case classes are caught looking like products
      case _: Tree | _: Type     => "" + x
      case x: TraversableOnce[_] => x mkString ", "
      case x: Product            => x.productIterator mkString ("(", ", ", ")")
      case _                     => "" + x
    }
    def ptIndent(x: Any) = ("" + x).replaceAll("\\n", "  ")
    def ptBlock(label: String, pairs: (String, Any)*): String = {
      if (pairs.isEmpty) label + "{ }"
      else {
        val width = (pairs map (_._1.length)).max
        val fmt   = "%-" + (width + 1) + "s %s"
        val strs  = pairs map { case (k, v) => fmt.format(k, to_s(v)) }

        strs.mkString(label + " {\n  ", "\n  ", "\n}")
      }
    }
    def ptLine(pairs: (String, Any)*): String = (
      pairs
              map { case (k,  v) => (k, to_s(v)) }
        filterNot { case (_,  v) => v == "" }
              map { case ("", v) => v ; case (k, v) => s"$k=$v" }
        mkString ", "
    )
    def ptTree(t: Tree) = t match {
      case PackageDef(pid, _)                => "package " + pid
      case ClassDef(_, name, tparams, _)     => "class " + name + str.brackets(tparams)
      case DefDef(_, name, tparams, _, _, _) => "def " + name + str.brackets(tparams)
      case ModuleDef(_, name, _)             => "object " + name
      case _                                 => to_s(t)
    }

    object str {
      def parentheses(xs: List[_]): String     = xs.mkString("(", ", ", ")")
      def brackets(xs: List[_]): String        = if (xs.isEmpty) "" else xs.mkString("[", ", ", "]")
      def tparams(tparams: List[Type]): String = brackets(tparams map debug)
      def parents(ps: List[Type]): String      = (ps map debug).mkString(" with ")
      def refine(defs: Scope): String          = defs.toList.mkString("{", " ;\n ", "}")
    }

    private def debug(tp: Type): String = tp match {
      case TypeRef(pre, sym, args)             => debug(pre) + "." + sym.nameString + str.tparams(args)
      case ThisType(sym)                       => sym.nameString + ".this"
      case SingleType(pre, sym)                => debug(pre) +"."+ sym.nameString +".type"
      case RefinedType(parents, defs)          => str.parents(parents) + str.refine(defs)
      case ClassInfoType(parents, defs, clazz) => "class "+ clazz.nameString + str.parents(parents) + str.refine(defs)
      case PolyType(tparams, result)           => str.brackets(tparams) + " " + debug(result)
      case TypeBounds(lo, hi)                  => ">: "+ debug(lo) +" <: "+ debug(hi)
      case tv @ TypeVar(_, _)                  => tv.toString
      case ExistentialType(tparams, qtpe)      => "forSome "+ str.brackets(tparams) + " " + debug(qtpe)
      case _                                   => "?"+tp.getClass.getName+"?"//tp.toString might produce cyclic error...
    }
    def debugString(tp: Type) = debug(tp)
  }
  def paramString(tp: Type)      = typeDebug.str parentheses (tp.params map (_.defString))
  def typeParamsString(tp: Type) = typeDebug.str brackets (tp.typeParams map (_.defString))
  def typeArgsString(tp: Type)   = typeDebug.str brackets (tp.typeArgs map (_.safeToString))
  def debugString(tp: Type)      = typeDebug debugString tp
}
