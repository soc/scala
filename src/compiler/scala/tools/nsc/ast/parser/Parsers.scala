/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

//todo: allow infix type patterns
//todo verify when stableId's should be just plain qualified type ids

package scala.tools.nsc
package ast.parser

import scala.collection.{ mutable, immutable }
import mutable.{ ListBuffer, StringBuilder }
import scala.reflect.internal.{ Precedence, ModifierFlags => Flags }
import scala.reflect.internal.Chars.{ isScalaLetter }
import scala.reflect.internal.util.{ SourceFile, Position }
import Tokens._
import util.FreshNameCreator

/** Historical note: JavaParsers started life as a direct copy of Parsers
 *  but at a time when that Parsers had been replaced by a different one.
 *  Later it was dropped and the original Parsers reinstated, leaving us with
 *  massive duplication between Parsers and JavaParsers.
 *
 *  This trait and the similar one for Scanners/JavaScanners represents
 *  the beginnings of a campaign against this latest incursion by Cutty
 *  McPastington and his army of very similar soldiers.
 */
trait ParsersCommon extends ScannersCommon { self =>
  val global : Global
  // the use of currentUnit in the parser should be avoided as it might
  // cause unexpected behaviour when you work with two units at the
  // same time; use Parser.unit instead
  import global.{currentUnit => _, _}

  def newLiteral(const: Any) = Literal(Constant(const))
  def literalUnit            = newLiteral(())

  /** This is now an abstract class, only to work around the optimizer:
   *  methods in traits are never inlined.
   */
  abstract class ParserCommon {
    val in: ScannerCommon
    def freshName(prefix: String): Name
    def freshTermName(prefix: String): TermName
    def freshTypeName(prefix: String): TypeName
    def deprecationWarning(off: Int, msg: String): Unit
    def accept(token: Int): Int

    /** Methods inParensOrError and similar take a second argument which, should
     *  the next token not be the expected opener (e.g. LPAREN) will be returned
     *  instead of the contents of the groupers.  However in all cases accept(LPAREN)
     *  will be called, so a parse error will still result.  If the grouping is
     *  optional, in.token should be tested before calling these methods.
     */
    @inline final def inParens[T](body: => T): T = {
      accept(LPAREN)
      val ret = body
      accept(RPAREN)
      ret
    }
    @inline final def inParensOrError[T](body: => T, alt: T): T =
      if (in.token == LPAREN) inParens(body)
      else { accept(LPAREN) ; alt }

    @inline final def inParensOrUnit[T](body: => Tree): Tree = inParensOrError(body, literalUnit)
    @inline final def inParensOrNil[T](body: => List[T]): List[T] = inParensOrError(body, Nil)

    @inline final def inBraces[T](body: => T): T = {
      accept(LBRACE)
      val ret = body
      accept(RBRACE)
      ret
    }
    @inline final def inBracesOrError[T](body: => T, alt: T): T =
      if (in.token == LBRACE) inBraces(body)
      else { accept(LBRACE) ; alt }

    @inline final def inBracesOrNil[T](body: => List[T]): List[T] = inBracesOrError(body, Nil)
    @inline final def inBracesOrUnit[T](body: => Tree): Tree = inBracesOrError(body, literalUnit)
    @inline final def dropAnyBraces[T](body: => T): T =
      if (in.token == LBRACE) inBraces(body)
      else body

    @inline final def inBrackets[T](body: => T): T = {
      accept(LBRACKET)
      val ret = body
      accept(RBRACKET)
      ret
    }

    /** Creates an actual Parens node (only used during parsing.)
     */
    @inline final def makeParens(body: => List[Tree]): Parens =
      Parens(inParens(if (in.token == RPAREN) Nil else body))
  }
}

/** Performs the following context-free rewritings:
 *
 *  <ol>
 *    <li>
 *      Places all pattern variables in Bind nodes. In a pattern, for
 *      identifiers `x`:<pre>
 *                 x  => x @ _
 *               x:T  => x @ (_ : T)</pre>
 *    </li>
 *    <li>Removes pattern definitions (PatDef's) as follows:
 *      If pattern is a simple (typed) identifier:<pre>
 *        <b>val</b> x = e     ==>  <b>val</b> x = e
 *        <b>val</b> x: T = e  ==>  <b>val</b> x: T = e</pre>
 *
 *      if there are no variables in pattern<pre>
 *        <b>val</b> p = e  ==>  e match (case p => ())</pre>
 *
 *      if there is exactly one variable in pattern<pre>
 *        <b>val</b> x_1 = e <b>match</b> (case p => (x_1))</pre>
 *
 *      if there is more than one variable in pattern<pre>
 *        <b>val</b> p = e  ==>  <b>private synthetic val</b> t$ = e <b>match</b> (case p => (x_1, ..., x_N))
 *                        <b>val</b> x_1 = t$._1
 *                        ...
 *                        <b>val</b> x_N = t$._N</pre>
 *    </li>
 *    <li>
 *       Removes function types as follows:<pre>
 *        (argtpes) => restpe   ==>   scala.Function_n[argtpes, restpe]</pre>
 *    </li>
 *    <li>
 *      Wraps naked case definitions in a match as follows:<pre>
 *        { cases }   ==>   (x => x.match {cases})<span style="font-family:normal;">, except when already argument to match</span></pre>
 *    </li>
 *  </ol>
 */
trait Parsers extends Scanners with MarkupParsers with ParsersCommon {
self =>
  val global: Global
  import global._

  case class OpInfo(source: SourceFile, lhs: Tree, operator: TermName, offset: Offset) {
    def precedence           = Precedence(operator.toString)
    def operatorPos          = Position.range(source, offset, offset, offset + operator.length)
    def finishPos(rhs: Tree) = lhs.pos union rhs.pos union operatorPos withPoint offset

    override def toString = s"""
      |OpInfo($source, $lhs, $operator, $offset) {
      |   precedence = $precedence
      |       lhsPos = ${lhs.pos.show}
      |  operatorPos = ${operatorPos.show}
      |}""".stripMargin.trim
  }

  class SourceFileParser(val source: SourceFile) extends Parser {

    /** The parse starting point depends on whether the source file is self-contained:
     *  if not, the AST will be supplemented.
     */
    def parseStartRule =
      if (source.isSelfContained) () => compilationUnit()
      else () => scriptBody()

    def newScanner(): Scanner = new SourceFileScanner(source)

    /** Scoping operator used to temporarily look into the future.
     *  Backs up scanner data before evaluating a block and restores it after.
     */
    def lookingAhead[T](body: => T): T = {
      val snapshot = (new ScannerData{}).copyFrom(in)
      in.nextToken()
      val res = body
      in copyFrom snapshot
      res
    }

    val in = newScanner()
    in.init()

    private val globalFresh = new FreshNameCreator.Default

    def unit = global.currentUnit
    def freshName(prefix: String): Name = freshTermName(prefix)
    def freshTermName(prefix: String): TermName = newTermName(globalFresh.newName(prefix))
    def freshTypeName(prefix: String): TypeName = newTypeName(globalFresh.newName(prefix))

    def o2p(offset: Int): Position = Position.offset(source, offset)
    def r2p(start: Int, mid: Int, end: Int): Position = rangePos(source, start, mid, end)

    // suppress warnings; silent abort on errors
    def warning(offset: Int, msg: String) {}
    def deprecationWarning(offset: Int, msg: String) {}

    def syntaxError(offset: Int, msg: String): Unit = throw new MalformedInput(offset, msg)
    def incompleteInputError(msg: String): Unit = throw new MalformedInput(source.content.length - 1, msg)

    object symbXMLBuilder extends SymbolicXMLBuilder(this, preserveWS = true) { // DEBUG choices
      val global: self.global.type = self.global
    }

    /** the markup parser
     * The first time this lazy val is accessed, we assume we were trying to parse an xml literal.
     * The current position is recorded for later error reporting if it turns out
     * that we don't have the xml library on the compilation classpath.
     */
    private[this] lazy val xmlp = {
      unit.encounteredXml(o2p(in.offset))
      new MarkupParser(this, preserveWS = true)
    }

    def xmlLiteral() : Tree = xmlp.xLiteral
    def xmlLiteralPattern() : Tree = xmlp.xLiteralPattern
  }

  class OutlineParser(source: SourceFile) extends SourceFileParser(source) {

    def skipBraces[T](body: T): T = {
      accept(LBRACE)
      var openBraces = 1
      while (in.token != EOF && openBraces > 0) {
        if (in.token == XMLSTART) xmlLiteral()
        else {
          if (in.token == LBRACE) openBraces += 1
          else if (in.token == RBRACE) openBraces -= 1
          in.nextToken()
        }
      }
      body
    }

    override def blockExpr(): Tree = skipBraces(EmptyTree)

    override def templateBody(isPre: Boolean) = skipBraces((emptyValDef, EmptyTree.asList))
  }

  class UnitParser(override val unit: global.CompilationUnit, patches: List[BracePatch]) extends SourceFileParser(unit.source) { uself =>
    def this(unit: global.CompilationUnit) = this(unit, Nil)

    override def newScanner() = new UnitScanner(unit, patches)

    override def freshTermName(prefix: String): TermName = unit.freshTermName(prefix)
    override def freshTypeName(prefix: String): TypeName = unit.freshTypeName(prefix)

    override def warning(offset: Int, msg: String) {
      unit.warning(o2p(offset), msg)
    }

    override def deprecationWarning(offset: Int, msg: String) {
      unit.deprecationWarning(o2p(offset), msg)
    }

    private var smartParsing = false
    @inline private def withSmartParsing[T](body: => T): T = {
      val saved = smartParsing
      smartParsing = true
      try body
      finally smartParsing = saved
    }
    def withPatches(patches: List[BracePatch]): UnitParser = new UnitParser(unit, patches)

    val syntaxErrors = new ListBuffer[(Int, String)]
    def showSyntaxErrors() =
      for ((offset, msg) <- syntaxErrors)
        unit.error(o2p(offset), msg)

    override def syntaxError(offset: Int, msg: String) {
      if (smartParsing) syntaxErrors += ((offset, msg))
      else unit.error(o2p(offset), msg)
    }

    override def incompleteInputError(msg: String) {
      val offset = source.content.length - 1
      if (smartParsing) syntaxErrors += ((offset, msg))
      else unit.incompleteInputError(o2p(offset), msg)
    }

    /** parse unit. If there are inbalanced braces,
     *  try to correct them and reparse.
     */
    def smartParse(): Tree = withSmartParsing {
      val firstTry = parse()
      if (syntaxErrors.isEmpty) firstTry
      else in.healBraces() match {
        case Nil      => showSyntaxErrors() ; firstTry
        case patches  => (this withPatches patches).parse()
      }
    }
  }

  final val Local = 0
  final val InBlock = 1
  final val InTemplate = 2

  // These symbols may not yet be loaded (e.g. in the ide) so don't go
  // through definitions to obtain the names.
  lazy val ScalaValueClassNames = Seq(tpnme.AnyVal,
      tpnme.Unit,
      tpnme.Boolean,
      tpnme.Byte,
      tpnme.Short,
      tpnme.Char,
      tpnme.Int,
      tpnme.Long,
      tpnme.Float,
      tpnme.Double)

  import nme.raw

  abstract class Parser extends ParserCommon { parser =>
    val in: Scanner

    def unit: CompilationUnit
    def freshName(prefix: String): Name
    def freshTermName(prefix: String): TermName
    def freshTypeName(prefix: String): TypeName
    def o2p(offset: Int): Position
    def r2p(start: Int, mid: Int, end: Int): Position

    /** Creates a range position from the given start offset to
     *  the value of in.lastOffset.
     */
    def rangeSince(start: Int): Position = r2p(start, start, in.lastOffset)

    /** Like in.skipToken, but returns a range position surrounding the skipped token.
     */
    def skipTokenRange(): Position = rangeSince(in.skipToken())

    /** whether a non-continuable syntax error has been seen */
    private var lastErrorOffset : Int = -1

    class ParserTreeBuilder extends UnitTreeBuilder {
      val global: self.global.type = self.global
      def unit = parser.unit
    }
    val treeBuilder = new ParserTreeBuilder
    import treeBuilder.{global => _, unit => _, _}

    /** The types of the context bounds of type parameters of the surrounding class
     */
    private var classContextBounds: List[Tree] = Nil
    @inline private def savingClassContextBounds[T](op: => T): T = {
      val saved = classContextBounds
      try op
      finally classContextBounds = saved
    }

    /** Are we inside the Scala package? Set for files that start with package scala
     */
    private var inScalaPackage = false
    private var currentPackage = ""
    def resetPackage() {
      inScalaPackage = false
      currentPackage = ""
    }
    private def inScalaRootPackage = inScalaPackage && currentPackage == "scala"

    def parseStartRule: () => Tree

    def parseRule[T](rule: this.type => T): T = {
      val t = rule(this)
      accept(EOF)
      t
    }

    /** This is the general parse entry point.
     */
    def parse(): Tree = parseRule(_.parseStartRule())

    /** This is alternative entry point for repl, script runner, toolbox and quasiquotes.
     */
    def parseStats(): List[Tree] = parseRule(_.templateStats())

    /** This is the parse entry point for code which is not self-contained, e.g.
     *  a script which is a series of template statements.  They will be
     *  swaddled in Trees until the AST is equivalent to the one returned
     *  by compilationUnit().
     */
    def scriptBody(): Tree = {
      val stmts = parseStats()

      def mainModuleName = newTermName(settings.script.value)
      /* If there is only a single object template in the file and it has a
       * suitable main method, we will use it rather than building another object
       * around it.  Since objects are loaded lazily the whole script would have
       * been a no-op, so we're not taking much liberty.
       */
      def searchForMain(): Option[Tree] = {
        /* Have to be fairly liberal about what constitutes a main method since
         * nothing has been typed yet - for instance we can't assume the parameter
         * type will look exactly like "Array[String]" as it could have been renamed
         * via import, etc.
         */
        def isMainMethod(t: Tree) = t match {
          case DefDef(_, nme.main, Nil, List(_), _, _)  => true
          case _                                        => false
        }
        /* For now we require there only be one top level object. */
        var seenModule = false
        val newStmts = stmts collect {
          case t @ Import(_, _) => t
          case md @ ModuleDef(mods, name, template) if !seenModule && (md exists isMainMethod) =>
            seenModule = true
            /* This slightly hacky situation arises because we have no way to communicate
             * back to the scriptrunner what the name of the program is.  Even if we were
             * willing to take the sketchy route of settings.script.value = progName, that
             * does not work when using fsc.  And to find out in advance would impose a
             * whole additional parse.  So instead, if the actual object's name differs from
             * what the script is expecting, we transform it to match.
             */
            if (name == mainModuleName) md
            else treeCopy.ModuleDef(md, mods, mainModuleName, template)
          case _ =>
            /* If we see anything but the above, fail. */
            return None
        }
        Some(makeEmptyPackage(0, newStmts))
      }

      if (mainModuleName == newTermName(ScriptRunner.defaultScriptMain))
        searchForMain() foreach { return _ }

      /*  Here we are building an AST representing the following source fiction,
       *  where `moduleName` is from -Xscript (defaults to "Main") and <stmts> are
       *  the result of parsing the script file.
       *
       *  {{{
       *  object moduleName {
       *    def main(argv: Array[String]): Unit = {
       *      val args = argv
       *      new AnyRef {
       *        stmts
       *      }
       *    }
       *  }
       *  }}}
       */
      def emptyInit   = DefDef(
        NoMods,
        nme.CONSTRUCTOR,
        Nil,
        ListOfNil,
        TypeTree(),
        Block(List(Apply(gen.mkSuperInitCall, Nil)), literalUnit)
      )

      // def main
      def mainParamType = AppliedTypeTree(Ident(tpnme.Array), List(Ident(tpnme.String)))
      def mainParameter = List(ValDef(Modifiers(Flags.PARAM), nme.argv, mainParamType, EmptyTree))
      def mainSetArgv   = List(ValDef(NoMods, nme.args, TypeTree(), Ident(nme.argv)))
      def mainDef       = DefDef(NoMods, nme.main, Nil, List(mainParameter), scalaDot(tpnme.Unit), Block(mainSetArgv, gen.mkAnonymousNew(stmts)))

      // object Main
      def moduleName  = newTermName(ScriptRunner scriptMain settings)
      def moduleBody  = Template(atInPos(scalaAnyRefConstr) :: Nil, emptyValDef, List(emptyInit, mainDef))
      def moduleDef   = ModuleDef(NoMods, moduleName, moduleBody)

      // package <empty> { ... }
      makeEmptyPackage(0, moduleDef :: Nil)
    }

/* --------------- PLACEHOLDERS ------------------------------------------- */

    /** The implicit parameters introduced by `_` in the current expression.
     *  Parameters appear in reverse order.
     */
    var placeholderParams: List[ValDef] = Nil

    /** The placeholderTypes introduced by `_` in the current type.
     *  Parameters appear in reverse order.
     */
    var placeholderTypes: List[TypeDef] = Nil

    def checkNoEscapingPlaceholders[T](op: => T): T = {
      val savedPlaceholderParams = placeholderParams
      val savedPlaceholderTypes = placeholderTypes
      placeholderParams = List()
      placeholderTypes = List()

      val res = op

      placeholderParams match {
        case vd :: _ =>
          syntaxError(vd.pos, "unbound placeholder parameter")
          placeholderParams = List()
        case _ =>
      }
      placeholderTypes match {
        case td :: _ =>
          syntaxError(td.pos, "unbound wildcard type")
          placeholderTypes = List()
        case _ =>
      }
      placeholderParams = savedPlaceholderParams
      placeholderTypes = savedPlaceholderTypes

      res
    }

    def placeholderTypeBoundary(op: => Tree): Tree = {
      val savedPlaceholderTypes = placeholderTypes
      placeholderTypes = List()
      var t = op
      if (!placeholderTypes.isEmpty && t.isInstanceOf[AppliedTypeTree]) {
        val expos = t.pos
        ensureNonOverlapping(t, placeholderTypes)
        t = atPos(expos) { ExistentialTypeTree(t, placeholderTypes.reverse) }
        placeholderTypes = List()
      }
      placeholderTypes = placeholderTypes ::: savedPlaceholderTypes
      t
    }

    def isWildcard(t: Tree): Boolean = t match {
      case Ident(name1) => !placeholderParams.isEmpty && name1 == placeholderParams.head.name
      case Typed(t1, _) => isWildcard(t1)
      case Annotated(t1, _) => isWildcard(t1)
      case _ => false
    }

/* ------------- ERROR HANDLING ------------------------------------------- */

    val assumedClosingParens = mutable.Map(RPAREN -> 0, RBRACKET -> 0, RBRACE -> 0)

    private var inFunReturnType = false
    @inline private def fromWithinReturnType[T](body: => T): T = {
      val saved = inFunReturnType
      inFunReturnType = true
      try body
      finally inFunReturnType = saved
    }

    protected def skip(targetToken: Int) {
      var nparens = 0
      var nbraces = 0
      while (true) {
        in.token match {
          case EOF =>
            return
          case SEMI =>
            if (nparens == 0 && nbraces == 0) return
          case NEWLINE =>
            if (nparens == 0 && nbraces == 0) return
          case NEWLINES =>
            if (nparens == 0 && nbraces == 0) return
          case RPAREN =>
            nparens -= 1
          case RBRACE =>
            if (nbraces == 0) return
            nbraces -= 1
          case LPAREN =>
            nparens += 1
          case LBRACE =>
            nbraces += 1
          case _ =>
        }
        if (targetToken == in.token && nparens == 0 && nbraces == 0) return
        in.nextToken()
      }
    }
    def warning(offset: Int, msg: String): Unit
    def incompleteInputError(msg: String): Unit
    def syntaxError(offset: Int, msg: String): Unit
    def syntaxError(msg: String, skipIt: Boolean): Unit = syntaxError(in.offset, msg, skipIt)
    def syntaxError(msg: String): Unit                  = syntaxError(msg, skipIt = false)

    private def syntaxError(pos: Position, msg: String, skipIt: Boolean): Unit = syntaxError(pos pointOrElse in.offset, msg, skipIt)
    private def syntaxError(pos: Position, msg: String): Unit                  = syntaxError(pos, msg, skipIt = false)

    def syntaxError(offset: Int, msg: String, skipIt: Boolean) {
      if (offset > lastErrorOffset) {
        syntaxError(offset, msg)
        // no more errors on this token.
        lastErrorOffset = in.offset
      }
      if (skipIt)
        skip(UNDEF)
    }

    def warning(msg: String) { warning(in.offset, msg) }

    def syntaxErrorAnd[T](msg: String)(and: T): T = { syntaxError(msg) ; and }
    def syntaxErrorAnd[T](msg: String, skipIt: Boolean)(and: T): T = { syntaxError(msg, skipIt) ; and }

    def syntaxErrorOrIncomplete(msg: String, skipIt: Boolean) {
      if (in.token == EOF)
        incompleteInputError(msg)
      else
        syntaxError(in.offset, msg, skipIt)
    }
    def syntaxErrorOrIncompleteAnd[T](msg: String, skipIt: Boolean)(and: T): T = {
      syntaxErrorOrIncomplete(msg, skipIt)
      and
    }

    def expectedMsgTemplate(expected: String, found: String): String = s"$expected expected but $found found."
    def expectedMsg(expected: Int, found: Int): String               = expectedMsgTemplate(token2string(expected), token2string(found))
    def expectedMsg(token: Int): String                              = expectedMsg(token, in.token)

    /** Consume one token of the specified type, or signal an error if it is not there. */
    def accept(token: Int): Int = {
      val offset = in.offset
      if (in.token != token) {
        syntaxErrorOrIncomplete(expectedMsg(token), skipIt = false)
        if ((token == RPAREN || token == RBRACE || token == RBRACKET))
          if (in.parenBalance(token) + assumedClosingParens(token) < 0)
            assumedClosingParens(token) += 1
          else
            skip(token)
        else
          skip(UNDEF)
      }
      if (in.token == token) in.nextToken()
      offset
    }
    /** If the given token is available for consumption, consume it and return true.
     *  Otherwise, do nothing and return false.
     */
    def acceptIfPresent(token: Int) = (in.token == token) && { accept(token) ; true }

    def acceptIdentIfPresent(name: Name): Boolean = isIdentOf(name) && { in.nextToken() ; true }

    /** {{{
     *  semi = nl {nl} | `;`
     *  nl  = `\n' // where allowed
     *  }}}
     */
    def acceptStatSep(): Unit = in.token match {
      case NEWLINE | NEWLINES => in.nextToken()
      case _                  => accept(SEMI)
    }
    def acceptStatSepOpt() =
      if (!isStatSeqEnd)
        acceptStatSep()

    def errorTypeTree    = setInPos(TypeTree() setType ErrorType)
    def errorTermTree    = setInPos(newLiteral(null))
    def errorPatternTree = setInPos(Ident(nme.WILDCARD))

    /** Check that type parameter is not by name or repeated. */
    def checkNotByNameOrVarargs(tpt: Tree) = {
      if (treeInfo isByNameParamType tpt)
        syntaxError(tpt.pos, "no by-name parameter type allowed here", skipIt = false)
      else if (treeInfo isRepeatedParamType tpt)
        syntaxError(tpt.pos, "no * parameter type allowed here", skipIt = false)
    }

    /** Check that tree is a legal clause of a forSome. */
    def checkLegalExistential(t: Tree) = t match {
      case TypeDef(_, _, _, TypeBoundsTree(_, _)) |
           ValDef(_, _, _, EmptyTree) | EmptyTree =>
             ;
      case _ =>
        syntaxError(t.pos, "not a legal existential clause", skipIt = false)
    }

/* -------------- TOKEN CLASSES ------------------------------------------- */

    def isModifier: Boolean = in.token match {
      case ABSTRACT | FINAL | SEALED | PRIVATE |
           PROTECTED | OVERRIDE | IMPLICIT | LAZY => true
      case _ => false
    }

    def isAnnotation: Boolean = in.token == AT

    def isCaseDefStart: Boolean = in.token == CASE

    def isLocalModifier: Boolean = in.token match {
      case ABSTRACT | FINAL | SEALED | IMPLICIT | LAZY => true
      case _ => false
    }

    def isTemplateIntro: Boolean = in.token match {
      case OBJECT | CASEOBJECT | CLASS | CASECLASS | TRAIT  => true
      case _                                                => false
    }
    def isDclIntro: Boolean = in.token match {
      case VAL | VAR | DEF | TYPE => true
      case _ => false
    }

    def isDefIntro = isTemplateIntro || isDclIntro
    def isTopLevelIntro = in.token match {
      case PACKAGE | IMPORT | AT => true
      case _                     => isTemplateIntro || isModifier
    }
    def isNumericLit: Boolean = in.token match {
      case INTLIT | LONGLIT | FLOATLIT | DOUBLELIT => true
      case _ => false
    }

    def isIdentExcept(except: Name) = isIdent && in.name != except
    def isIdentOf(name: Name)       = isIdent && in.name == name

    def isUnaryOp = isIdent && raw.isUnary(in.name)
    def isRawStar = isIdentOf(raw.STAR)
    def isRawBar  = isIdentOf(raw.BAR)

    def isIdent = in.token == IDENTIFIER || in.token == BACKQUOTED_IDENT

    def isLiteralToken(token: Int) = token match {
      case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT |
           STRINGLIT | INTERPOLATIONID | SYMBOLLIT | TRUE | FALSE | NULL => true
      case _                                                        => false
    }
    def isLiteral = isLiteralToken(in.token)

    def isExprIntroToken(token: Int): Boolean = isLiteralToken(token) || (token match {
      case IDENTIFIER | BACKQUOTED_IDENT |
           THIS | SUPER | IF | FOR | NEW | USCORE | TRY | WHILE |
           DO | RETURN | THROW | LPAREN | LBRACE | XMLSTART => true
      case _ => false
    })

    def isExprIntro: Boolean = isExprIntroToken(in.token)

    def isTypeIntroToken(token: Int): Boolean = token match {
      case IDENTIFIER | BACKQUOTED_IDENT | THIS |
           SUPER | USCORE | LPAREN | AT => true
      case _ => false
    }

    def isStatSeqEnd = in.token == RBRACE || in.token == EOF

    def isStatSep(token: Int): Boolean =
      token == NEWLINE || token == NEWLINES || token == SEMI

    def isStatSep: Boolean = isStatSep(in.token)


/* --------- COMMENT AND ATTRIBUTE COLLECTION ----------------------------- */

    /** A hook for joining the comment associated with a definition.
     *  Overridden by scaladoc.
     */
    def joinComment(trees: => List[Tree]): List[Tree] = trees

/* ---------- TREE CONSTRUCTION ------------------------------------------- */

    def atPos[T <: Tree](start: Int)(t: T): T                       = atPos[T](start, start)(t)
    def atPos[T <: Tree](start: Int, point: Int)(t: T): T           = atPos[T](start, point, in.lastOffset max start)(t)
    def atPos[T <: Tree](start: Int, point: Int, end: Int)(t: T): T = atPos(r2p(start, point, end))(t)
    def atPos[T <: Tree](pos: Position)(t: T): T                    = global.atPos(pos)(t)

    def atInPos[T <: Tree](t: T): T  = atPos(o2p(in.offset))(t)
    def setInPos[T <: Tree](t: T): T = t setPos o2p(in.offset)

    /** Use with caution. */
    def peekahead(): Unit = {
      in.prev copyFrom in
      in.nextToken()
    }
    def pushback(): Unit = {
      in.next copyFrom in
      in copyFrom in.prev
    }

    /** Convert tree to formal parameter list. */
    def convertToParams(tree: Tree): List[ValDef] = tree match {
      case Parens(ts) => ts map convertToParam
      case _          => List(convertToParam(tree))
    }

    /** Convert tree to formal parameter. */
    def convertToParam(tree: Tree): ValDef = atPos(tree.pos) {
      def removeAsPlaceholder(name: Name) {
        placeholderParams = placeholderParams filter (_.name != name)
      }
      tree match {
        case Ident(name) =>
          removeAsPlaceholder(name)
          makeParam(name.toTermName, TypeTree() setPos o2p(tree.pos.end))
        case Typed(Ident(name), tpe) if tpe.isType => // get the ident!
          removeAsPlaceholder(name)
          makeParam(name.toTermName, tpe)
        case _ =>
          syntaxError(tree.pos, "not a legal formal parameter", skipIt = false)
          makeParam(nme.ERROR, errorTypeTree setPos o2p(tree.pos.end))
      }
    }

    /** Convert (qual)ident to type identifier. */
    def convertToTypeId(tree: Tree): Tree = atPos(tree.pos) {
      convertToTypeName(tree) getOrElse {
        syntaxError(tree.pos, "identifier expected", skipIt = false)
        errorTypeTree
      }
    }

    /** {{{ part { `sep` part } }}},or if sepFirst is true, {{{ { `sep` part } }}}. */
    final def tokenSeparated[T](separator: Int, sepFirst: Boolean, part: => T): List[T] = {
      val ts = new ListBuffer[T]
      if (!sepFirst)
        ts += part

      while (acceptIfPresent(separator))
        ts += part

      ts.toList
    }
    @inline final def commaSeparated[T](part: => T): List[T] = tokenSeparated(COMMA, sepFirst = false, part)
    @inline final def caseSeparated[T](part: => T): List[T] = tokenSeparated(CASE, sepFirst = true, part)
    def readAnnots(part: => Tree): List[Tree] = tokenSeparated(AT, sepFirst = true, part)

/* --------- OPERAND/OPERATOR STACK --------------------------------------- */

    /** Modes for infix types. */
    object InfixMode extends Enumeration {
      val FirstOp, LeftOp, RightOp = Value
    }

    var opstack: List[OpInfo] = Nil
    private def opHead = opstack.head
    private def headPrecedence = opHead.precedence
    private def popOpInfo(): OpInfo = try opHead finally opstack = opstack.tail
    private def pushOpInfo(top: Tree) {
      val opinfo = OpInfo(unit.source, top, in.name, in.offset)
      opstack ::= opinfo
      ident()
    }

    def checkHeadAssoc(leftAssoc: Boolean) { checkAssoc(opHead.offset, opHead.operator, leftAssoc) }
    def checkAssoc(offset: Int, op: Name, leftAssoc: Boolean) {
      if (treeInfo.isLeftAssoc(op) != leftAssoc)
        syntaxError(offset, "left- and right-associative operators with same precedence may not be mixed", skipIt = false)
    }

    def finishPostfixOp(start: Int, base: List[OpInfo], opinfo: OpInfo): Tree = {
      val od = stripParens(reduceExprStack(base, opinfo.lhs))
      makePostfixSelect(start, opinfo.offset, od, opinfo.operator)
    }

    def finishBinaryOp(isExpr: Boolean, opinfo: OpInfo, rhs: Tree): Tree =
      atPos(opinfo finishPos rhs)(makeBinop(isExpr, opinfo.lhs, opinfo.operator, rhs, opinfo.operatorPos))

    def reduceExprStack(base: List[OpInfo], top: Tree): Tree    = reduceStack(isExpr = true, base, top)
    def reducePatternStack(base: List[OpInfo], top: Tree): Tree = reduceStack(isExpr = false, base, top)

    def reduceStack(isExpr: Boolean, base: List[OpInfo], top: Tree): Tree = {
      val opPrecedence = if (isIdent) Precedence(in.name.toString) else Precedence(0)
      val leftAssoc    = !isIdent || (treeInfo isLeftAssoc in.name)

      reduceStack(isExpr, base, top, opPrecedence, leftAssoc)
    }

    def reduceStack(isExpr: Boolean, base: List[OpInfo], top: Tree, opPrecedence: Precedence, leftAssoc: Boolean): Tree = {
      def isDone          = opstack == base
      def lowerPrecedence = !isDone && (opPrecedence < headPrecedence)
      def samePrecedence  = !isDone && (opPrecedence == headPrecedence)
      def canReduce       = lowerPrecedence || leftAssoc && samePrecedence

      if (samePrecedence)
        checkHeadAssoc(leftAssoc)

      def loop(top: Tree): Tree =
        if (canReduce) loop(finishBinaryOp(isExpr, popOpInfo(), top)) else top

      loop(top)
    }

/* -------- IDENTIFIERS AND LITERALS ------------------------------------------- */

    /** Methods which implicitly propagate the context in which they were
     *  called: either in a pattern context or not.  Formerly, this was
     *  threaded through numerous methods as boolean isPattern.
     */
    trait PatternContextSensitive {
      /** {{{
       *  ArgType       ::=  Type
       *  }}}
       */
      def argType(): Tree
      def functionArgType(): Tree

      private def tupleInfixType(start: Int) = {
        in.nextToken()
        if (acceptIfPresent(RPAREN))
          atPos(start, accept(ARROW)) { makeFunctionTypeTree(Nil, typ()) }
        else {
          val ts = functionTypes()
          accept(RPAREN)
          if (in.token == ARROW)
            atPos(start, in.skipToken()) { makeFunctionTypeTree(ts, typ()) }
          else {
            ts foreach checkNotByNameOrVarargs
            val tuple = atPos(start) { makeTupleType(ts, flattenUnary = true) }
            infixTypeRest(
              compoundTypeRest(
                annotTypeRest(
                  simpleTypeRest(
                    tuple))),
              InfixMode.FirstOp
            )
          }
        }
      }
      private def makeExistentialTypeTree(t: Tree) = {
        val whereClauses = refinement()
        whereClauses foreach checkLegalExistential
        ExistentialTypeTree(t, whereClauses)
      }

      /** {{{
       *  Type ::= InfixType `=>' Type
       *         | `(' [`=>' Type] `)' `=>' Type
       *         | InfixType [ExistentialClause]
       *  ExistentialClause ::= forSome `{' ExistentialDcl {semi ExistentialDcl}} `}'
       *  ExistentialDcl    ::= type TypeDcl | val ValDcl
       *  }}}
       */
      def typ(): Tree = placeholderTypeBoundary {
        val start = in.offset
        val t =
          if (in.token == LPAREN) tupleInfixType(start)
          else infixType(InfixMode.FirstOp)

        in.token match {
          case ARROW    => atPos(start, in.skipToken()) { makeFunctionTypeTree(List(t), typ()) }
          case FORSOME  => atPos(start, in.skipToken()) { makeExistentialTypeTree(t) }
          case _        => t
        }
      }

      /** {{{
       *  TypeArgs    ::= `[' ArgType {`,' ArgType} `]'
       *  }}}
       */
      def typeArgs(): List[Tree] = inBrackets(types())

      /** {{{
       *  AnnotType        ::=  SimpleType {Annotation}
       *  }}}
       */
      def annotType(): Tree = placeholderTypeBoundary { annotTypeRest(simpleType()) }

      /** {{{
       *  SimpleType       ::=  SimpleType TypeArgs
       *                     |  SimpleType `#' Id
       *                     |  StableId
       *                     |  Path `.' type
       *                     |  `(' Types `)'
       *                     |  WildcardType
       *  }}}
       */
      def simpleType(): Tree = {
        val start = in.offset
        simpleTypeRest(in.token match {
          case LPAREN   => atPos(start)(makeTupleType(inParens(types()), flattenUnary = true))
          case USCORE   => wildcardType(in.skipToken())
          case _        =>
            path(thisOK = false, typeOK = true) match {
              case r @ SingletonTypeTree(_) => r
              case r => convertToTypeId(r)
            }
        })
      }

      private def typeProjection(t: Tree): Tree = {
        val hashOffset = in.skipToken()
        val nameOffset = in.offset
        val name       = identForType(skipIt = false)
        val point      = if (name == tpnme.ERROR) hashOffset else nameOffset
        atPos(t.pos.start, point)(SelectFromTypeTree(t, name))
      }
      def simpleTypeRest(t: Tree): Tree = in.token match {
        case HASH     => simpleTypeRest(typeProjection(t))
        case LBRACKET => simpleTypeRest(atPos(t.pos.start, t.pos.point)(AppliedTypeTree(t, typeArgs())))
        case _        => t
      }

      /** {{{
       *  CompoundType ::= AnnotType {with AnnotType} [Refinement]
       *                |  Refinement
       *  }}}
       */
      def compoundType(): Tree = compoundTypeRest(
        if (in.token == LBRACE) atInPos(scalaAnyRefConstr)
        else annotType()
      )

      def compoundTypeRest(t: Tree): Tree = {
        val types = t :: tokenSeparated(WITH, sepFirst = true, annotType())
        newLineOptWhenFollowedBy(LBRACE)
        val braceOffset   = in.offset
        val hasRefinement = in.token == LBRACE
        val refinements   = if (hasRefinement) refinement() else Nil
        // Warn if they are attempting to refine Unit; we can't be certain it's
        // scala.Unit they're refining because at this point all we have is an
        // identifier, but at a later stage we lose the ability to tell an empty
        // refinement from no refinement at all.  See bug #284.
        if (hasRefinement) types match {
          case Ident(name) :: Nil if name endsWith "Unit" => warning(braceOffset, "Detected apparent refinement of Unit; are you missing an '=' sign?")
          case _                                          =>
        }
        // The second case includes an empty refinement - refinements is empty, but
        // it still gets a CompoundTypeTree.
        types match {
          case tp :: Nil if !hasRefinement => tp  // single type, no refinement, already positioned
          case tps                         => atPos(t.pos.start)(CompoundTypeTree(Template(tps, emptyValDef, refinements)))
        }
      }

      def infixTypeRest(t: Tree, mode: InfixMode.Value): Tree = {
        if (isIdentExcept(nme.STAR)) {
          val opOffset = in.offset
          val leftAssoc = treeInfo.isLeftAssoc(in.name)
          if (mode != InfixMode.FirstOp) checkAssoc(opOffset, in.name, leftAssoc = mode == InfixMode.LeftOp)
          val op = identForType()
          val tycon = atPos(opOffset) { Ident(op) }
          newLineOptWhenFollowing(isTypeIntroToken)
          def mkOp(t1: Tree) = atPos(t.pos.start, opOffset) { AppliedTypeTree(tycon, List(t, t1)) }
          if (leftAssoc)
            infixTypeRest(mkOp(compoundType()), InfixMode.LeftOp)
          else
            mkOp(infixType(InfixMode.RightOp))
        } else t
      }

      /** {{{
       *  InfixType ::= CompoundType {id [nl] CompoundType}
       *  }}}
       */
      def infixType(mode: InfixMode.Value): Tree =
        placeholderTypeBoundary { infixTypeRest(compoundType(), mode) }

      /** {{{
       *  Types ::= Type {`,' Type}
       *  }}}
       */
      def types(): List[Tree] = commaSeparated(argType())
      def functionTypes(): List[Tree] = commaSeparated(functionArgType())
    }

    /** Assumed (provisionally) to be TermNames. */
    def ident(skipIt: Boolean): Name = (
      if (isIdent) rawIdent().encode
      else syntaxErrorOrIncompleteAnd(expectedMsg(IDENTIFIER), skipIt)(nme.ERROR)
    )

    def ident(): Name = ident(skipIt = true)
    def rawIdent(): Name = try in.name finally in.nextToken()

    /** For when it's known already to be a type name. */
    def identForType(): TypeName                = ident().toTypeName
    def identForType(skipIt: Boolean): TypeName = ident(skipIt).toTypeName
    def identForTerm(): TermName                = ident().toTermName

    def selector(t: Tree): Tree = {
      val point = in.offset
      //assert(t.pos.isDefined, t)
      if (t != EmptyTree)
        Select(t, ident(skipIt = false)) setPos r2p(t.pos.start, point, in.lastOffset)
      else
        errorTermTree // has already been reported
    }

    /** {{{
     *  Path       ::= StableId
     *              |  [Ident `.'] this
     *  AnnotType ::= Path [`.' type]
     *  }}}
     */
    def path(thisOK: Boolean, typeOK: Boolean): Tree = {
      val start = in.offset
      var t: Tree = null
      if (acceptIfPresent(THIS)) {
        t = atPos(start) { This(tpnme.EMPTY) }
        if (!thisOK || in.token == DOT) {
          t = selectors(t, typeOK, accept(DOT))
        }
      }
      else if (acceptIfPresent(SUPER)) {
        t = atPos(start) { Super(This(tpnme.EMPTY), mixinQualifierOpt()) }
        accept(DOT)
        t = selector(t)
        if (in.token == DOT) t = selectors(t, typeOK, in.skipToken())
      }
      else {
        val tok = in.token
        val name = ident()
        t = atPos(start) {
          if (tok == BACKQUOTED_IDENT) Ident(name) updateAttachment BackquotedIdentifierAttachment
          else Ident(name)
        }
        if (in.token == DOT) {
          val dotOffset = in.skipToken()
          if (acceptIfPresent(THIS)) {
            t = atPos(start) { This(name.toTypeName) }
            if (!thisOK || in.token == DOT)
              t = selectors(t, typeOK, accept(DOT))
          }
          else if (acceptIfPresent(SUPER)) {
            t = atPos(start) { Super(This(name.toTypeName), mixinQualifierOpt()) }
            accept(DOT)
            t = selector(t)
            if (in.token == DOT) t = selectors(t, typeOK, in.skipToken())
          }
          else {
            t = selectors(t, typeOK, dotOffset)
          }
        }
      }
      t
    }

    def selectors(t: Tree, typeOK: Boolean, dotOffset: Int): Tree =
      if (typeOK && acceptIfPresent(TYPE))
        atPos(t.pos.start, dotOffset) { SingletonTypeTree(t) }
      else {
        val t1 = selector(t)
        if (in.token == DOT) { selectors(t1, typeOK, in.skipToken()) }
        else t1
      }

    /** {{{
    *   MixinQualifier ::= `[' Id `]'
    *   }}}
    */
    def mixinQualifierOpt(): TypeName =
      if (in.token == LBRACKET) inBrackets(identForType())
      else tpnme.EMPTY

    /** {{{
     *  StableId ::= Id
     *            |  Path `.' Id
     *            |  [id `.'] super [`[' id `]']`.' id
     *  }}}
     */
    def stableId(): Tree =
      path(thisOK = false, typeOK = false)

    /** {{{
    *   QualId ::= Id {`.' Id}
    *   }}}
    */
    def qualId(): Tree = {
      val start = in.offset
      val id = atPos(start) { Ident(ident()) }
      if (in.token == DOT) { selectors(id, typeOK = false, in.skipToken()) }
      else id
    }
    /** Calls `qualId()` and manages some package state. */
    private def pkgQualId() = {
      if (isIdentOf(nme.scala_))
        inScalaPackage = true

      val pkg = qualId()
      newLineOptWhenFollowedBy(LBRACE)

      if (currentPackage == "") currentPackage = pkg.toString
      else currentPackage = currentPackage + "." + pkg

      pkg
    }

    /** {{{
     *  SimpleExpr    ::= literal
     *                  | symbol
     *                  | null
     *  }}}
     */
    def literal(isNegated: Boolean = false, inPattern: Boolean = false, start: Int = in.offset): Tree = atPos(start) {
      def finish(value: Any): Tree = try newLiteral(value) finally in.nextToken()
      if (in.token == SYMBOLLIT)
        Apply(scalaDot(nme.Symbol), List(finish(in.strVal)))
      else if (in.token == INTERPOLATIONID)
        interpolatedString(inPattern = inPattern)
      else finish(in.token match {
        case CHARLIT                => in.charVal
        case INTLIT                 => in.intVal(isNegated).toInt
        case LONGLIT                => in.intVal(isNegated)
        case FLOATLIT               => in.floatVal(isNegated).toFloat
        case DOUBLELIT              => in.floatVal(isNegated)
        case STRINGLIT | STRINGPART => in.strVal.intern()
        case TRUE                   => true
        case FALSE                  => false
        case NULL                   => null
        case _                      => syntaxErrorOrIncompleteAnd("illegal literal", skipIt = true)(null)
      })
    }

    /** Handle placeholder syntax.
     *  If evaluating the tree produces placeholders, then make it a function.
     */
    private def withPlaceholders(tree: =>Tree, isAny: Boolean): Tree = {
      val savedPlaceholderParams = placeholderParams
      placeholderParams = List()
      var res = tree
      if (placeholderParams.nonEmpty && !isWildcard(res)) {
        res = atPos(res.pos)(Function(placeholderParams.reverse, res))
        if (isAny) placeholderParams foreach (_.tpt match {
          case tpt @ TypeTree() => tpt setType definitions.AnyTpe
          case _                => // some ascription
        })
        placeholderParams = List()
      }
      placeholderParams = placeholderParams ::: savedPlaceholderParams
      res
    }

    /** Consume a USCORE and create a fresh synthetic placeholder param. */
    private def freshPlaceholder(): Tree = {
      val pname = freshName("x$")
      val id = atPos(accept(USCORE))(Ident(pname))
      val param = atPos(id.pos.focus)(gen.mkSyntheticParam(pname.toTermName))
      placeholderParams ::= param
      id
    }

    private def interpolatedString(inPattern: Boolean): Tree = {
      def errpolation() = syntaxErrorOrIncompleteAnd("error in interpolated string: identifier or block expected",
                                                     skipIt = true)(EmptyTree)
      // Like Swiss cheese, with holes
      def stringCheese: Tree = atPos(in.offset) {
        val start = in.offset
        val interpolator = in.name

        val partsBuf = new ListBuffer[Tree]
        val exprBuf = new ListBuffer[Tree]
        in.nextToken()
        while (in.token == STRINGPART) {
          partsBuf += literal()
          exprBuf += (
            if (inPattern) dropAnyBraces(pattern())
            else in.token match {
              case IDENTIFIER => atPos(in.offset)(Ident(ident()))
              //case USCORE   => freshPlaceholder()  // ifonly etapolation
              case LBRACE     => expr()              // dropAnyBraces(expr0(Local))
              case THIS       => in.nextToken(); atPos(in.offset)(This(tpnme.EMPTY))
              case _          => errpolation()
            }
          )
        }
        if (in.token == STRINGLIT) partsBuf += literal()

        val t1 = atPos(o2p(start)) { Ident(nme.StringContext) }
        val t2 = atPos(start) { Apply(t1, partsBuf.toList) }
        t2 setPos t2.pos.makeTransparent
        val t3 = Select(t2, interpolator) setPos t2.pos
        atPos(start) { Apply(t3, exprBuf.toList) }
      }
      if (inPattern) stringCheese
      else withPlaceholders(stringCheese, isAny = true) // strinterpolator params are Any* by definition
    }

/* ------------- NEW LINES ------------------------------------------------- */

    def newLineOpt() {
      acceptIfPresent(NEWLINE)
    }

    def newLinesOpt() {
      if (in.token == NEWLINE || in.token == NEWLINES)
        in.nextToken()
    }

    def newLineOptWhenFollowedBy(token: Int) {
      // note: next is defined here because current == NEWLINE
      if (in.token == NEWLINE && in.next.token == token) newLineOpt()
    }

    def newLineOptWhenFollowing(p: Int => Boolean) {
      // note: next is defined here because current == NEWLINE
      if (in.token == NEWLINE && p(in.next.token)) newLineOpt()
    }

/* ------------- TYPES ---------------------------------------------------- */

    /** {{{
     *  TypedOpt ::= [`:' Type]
     *  }}}
     */
    def typedOpt(): Tree = if (acceptIfPresent(COLON)) typ() else TypeTree()

    def typeOrInfixType(location: Int): Tree =
      if (location == Local) typ()
      else startInfixType()

    def annotTypeRest(t: Tree): Tree =
      (t /: annotations(skipNewLines = false)) (makeAnnotated)

    /** {{{
     *  WildcardType ::= `_' TypeBounds
     *  }}}
     */
    def wildcardType(start: Int) = {
      val pname = freshTypeName("_$")
      val t = atPos(start)(Ident(pname))
      val bounds = typeBounds()
      val param = atUnionPos(t, bounds) { makeSyntheticTypeParam(pname, bounds) }
      placeholderTypes = param :: placeholderTypes
      t
    }

/* ----------- EXPRESSIONS ------------------------------------------------ */

    def condExpr(): Tree = in.token match {
      case LPAREN => inParens(expr())
      case _      => syntaxErrorOrIncompleteAnd("parenthesized conditional expression expected", skipIt = false)(newLiteral(true))
    }

    /* hook for IDE, unlike expression can be stubbed
     * don't use for any tree that can be inspected in the parser!
     */
    def statement(location: Int): Tree = expr(location) // !!! still needed?

    /** {{{
     *  Expr       ::= (Bindings | [`implicit'] Id | `_')  `=>' Expr
     *               | Expr1
     *  ResultExpr ::= (Bindings | Id `:' CompoundType) `=>' Block
     *               | Expr1
     *  Expr1      ::= if `(' Expr `)' {nl} Expr [[semi] else Expr]
     *               | try (`{' Block `}' | Expr) [catch `{' CaseClauses `}'] [finally Expr]
     *               | while `(' Expr `)' {nl} Expr
     *               | do Expr [semi] while `(' Expr `)'
     *               | for (`(' Enumerators `)' | `{' Enumerators `}') {nl} [yield] Expr
     *               | throw Expr
     *               | return [Expr]
     *               | [SimpleExpr `.'] Id `=' Expr
     *               | SimpleExpr1 ArgumentExprs `=' Expr
     *               | PostfixExpr Ascription
     *               | PostfixExpr match `{' CaseClauses `}'
     *  Bindings   ::= `(' [Binding {`,' Binding}] `)'
     *  Binding    ::= (Id | `_') [`:' Type]
     *  Ascription ::= `:' CompoundType
     *               | `:' Annotation {Annotation}
     *               | `:' `_' `*'
     *  }}}
     */
    def expr(): Tree = expr(Local)

    def expr(location: Int): Tree = withPlaceholders(expr0(location), isAny = false)

    def expr0(location: Int): Tree = (in.token: @scala.annotation.switch) match {
      case IF =>
        def parseIf = atPos(in.skipToken()) {
          val cond = condExpr()
          newLinesOpt()
          val thenp = expr()
          val elsep = if (acceptIfPresent(ELSE)) expr() else literalUnit
          If(cond, thenp, elsep)
        }
        parseIf
      case TRY =>
        def parseTry = atPos(in.skipToken()) {
          val body = in.token match {
            case LBRACE => inBracesOrUnit(block())
            case LPAREN => inParensOrUnit(expr())
            case _      => expr()
          }
          def catchFromExpr() = List(makeCatchFromExpr(expr()))
          val catches: List[CaseDef] = (
            if (!acceptIfPresent(CATCH)) Nil else {
              if (in.token != LBRACE) catchFromExpr()
              else inBracesOrNil {
                if (isCaseDefStart) caseClauses()
                else catchFromExpr()
              }
            }
          )
          val finalizer = if (acceptIfPresent(FINALLY)) expr() else EmptyTree
          Try(body, catches, finalizer)
        }
        parseTry
      case WHILE =>
        def parseWhile = {
          val start = in.offset
          atPos(in.skipToken()) {
            val cond = condExpr()
            newLinesOpt()
            val body = expr()
            makeWhile(cond, body)
          }
        }
        parseWhile
      case DO =>
        def parseDo = {
          atPos(in.skipToken()) {
            val lname: Name = freshTermName(nme.DO_WHILE_PREFIX)
            val body = expr()
            if (isStatSep) in.nextToken()
            accept(WHILE)
            val cond = condExpr()
            makeDoWhile(lname.toTermName, body, cond)
          }
        }
        parseDo
      case FOR =>
        val start = in.skipToken()
        def parseFor = atPos(start) {
          val enums = (
            if (in.token == LBRACE) inBracesOrNil(enumerators())
            else inParensOrNil(enumerators())
          )
          newLinesOpt()
          if (acceptIfPresent(YIELD))
            makeForYield(enums, expr())
          else
            makeFor(enums, expr())
        }
        def adjustStart(tree: Tree) =
          if (tree.pos.isRange && start < tree.pos.start)
            tree setPos tree.pos.withStart(start)
          else tree
        adjustStart(parseFor)
      case RETURN =>
        def parseReturn =
          atPos(in.skipToken()) {
            Return(if (isExprIntro) expr() else literalUnit)
          }
        parseReturn
      case THROW =>
        def parseThrow =
          atPos(in.skipToken()) {
            Throw(expr())
          }
        parseThrow
      case IMPLICIT =>
        implicitClosure(in.skipToken(), location)
      case _ =>
        def parseOther = {
          var t = postfixExpr()
          if (in.token == EQUALS) {
            t match {
              case Ident(_) | Select(_, _) | Apply(_, _) =>
                t = atPos(t.pos.start, in.skipToken()) { makeAssign(t, expr()) }
              case _ =>
            }
          } else if (in.token == COLON) {
            t = stripParens(t)
            val colonPos = in.skipToken()
            if (in.token == USCORE) {
              //todo: need to handle case where USCORE is a wildcard in a type
              val uscorePos = in.skipToken()
              if (isIdentOf(nme.STAR)) {
                in.nextToken()
                t = atPos(t.pos.start, colonPos) {
                  Typed(t, atPos(uscorePos) { Ident(tpnme.WILDCARD_STAR) })
                }
              } else {
                syntaxErrorOrIncomplete("`*' expected", skipIt = true)
              }
            } else if (isAnnotation) {
              t = (t /: annotations(skipNewLines = false))(makeAnnotated)
            } else {
              t = atPos(t.pos.start, colonPos) {
                val tpt = typeOrInfixType(location)
                if (isWildcard(t))
                  (placeholderParams: @unchecked) match {
                    case (vd @ ValDef(mods, name, _, _)) :: rest =>
                      placeholderParams = treeCopy.ValDef(vd, mods, name, tpt.duplicate, EmptyTree) :: rest
                  }
                // this does not correspond to syntax, but is necessary to
                // accept closures. We might restrict closures to be between {...} only.
                Typed(t, tpt)
              }
            }
          } else if (in.token == MATCH) {
            t = atPos(t.pos.start, in.skipToken())(Match(stripParens(t), inBracesOrNil(caseClauses())))
          }
          // in order to allow anonymous functions as statements (as opposed to expressions) inside
          // templates, we have to disambiguate them from self type declarations - bug #1565
          // The case still missed is unparenthesized single argument, like "x: Int => x + 1", which
          // may be impossible to distinguish from a self-type and so remains an error.  (See #1564)
          def lhsIsTypedParamList() = t match {
            case Parens(xs) if xs.forall(isTypedParam) => true
            case _ => false
          }
          if (in.token == ARROW && (location != InTemplate || lhsIsTypedParamList)) {
            t = atPos(t.pos.start, in.skipToken()) {
              Function(convertToParams(t), if (location != InBlock) expr() else block())
            }
          }
          stripParens(t)
        }
        parseOther
    }

    def isTypedParam(t: Tree) = t.isInstanceOf[Typed]

    /** {{{
     *  Expr ::= implicit Id => Expr
     *  }}}
     */

    def implicitClosure(start: Int, location: Int): Tree = {
      val param0 = convertToParam {
        atPos(in.offset) {
          Ident(ident()) match {
            case expr if acceptIfPresent(COLON) => Typed(expr, typeOrInfixType(location))
            case expr                           => expr
          }
        }
      }
      val param = copyValDef(param0)(mods = param0.mods | Flags.IMPLICIT)
      atPos(start, in.offset) {
        accept(ARROW)
        Function(List(param), if (location != InBlock) expr() else block())
      }
    }

    /** {{{
     *  PostfixExpr   ::= InfixExpr [Id [nl]]
     *  InfixExpr     ::= PrefixExpr
     *                  | InfixExpr Id [nl] InfixExpr
     *  }}}
     */
    def postfixExpr(): Tree = {
      val start = in.offset
      val base  = opstack

      def loop(top: Tree): Tree = if (!isIdent) top else {
        pushOpInfo(reduceExprStack(base, top))
        newLineOptWhenFollowing(isExprIntroToken)
        if (isExprIntro)
          prefixExpr() match {
            case EmptyTree => reduceExprStack(base, top)
            case next      => loop(next)
          }
        else finishPostfixOp(start, base, popOpInfo())
      }

      reduceExprStack(base, loop(prefixExpr()))
    }

    /** {{{
     *  PrefixExpr   ::= [`-' | `+' | `~' | `!' | `&'] SimpleExpr
     *  }}}
     */
    def prefixExpr(): Tree = {
      if (isUnaryOp) {
        atPos(in.offset) {
          val name = nme.toUnaryName(rawIdent().toTermName)
          if (name == nme.UNARY_- && isNumericLit)
            simpleExprRest(literal(isNegated = true), canApply = true)
          else
            Select(stripParens(simpleExpr()), name)
        }
      }
      else simpleExpr()
    }
    def xmlLiteral(): Tree

    /** {{{
     *  SimpleExpr    ::= new (ClassTemplate | TemplateBody)
     *                  |  BlockExpr
     *                  |  SimpleExpr1 [`_']
     *  SimpleExpr1   ::= literal
     *                  |  xLiteral
     *                  |  Path
     *                  |  `(' [Exprs] `)'
     *                  |  SimpleExpr `.' Id
     *                  |  SimpleExpr TypeArgs
     *                  |  SimpleExpr1 ArgumentExprs
     *  }}}
     */
    def simpleExpr(): Tree = {
      val canApply = in.token match {
        case LBRACE | NEW => false
        case _            => true
      }
      def mkNew(): Tree = {
        val npos                   = skipTokenRange()
        val tstart                 = in.offset
        val (parents, self, stats) = template()
        val cpos                   = rangeSince(tstart)

        gen.mkNew(parents, self, stats, npos, cpos)
      }
      val t = in.token match {
        case _ if isLiteral                               => literal()
        case XMLSTART                                     => xmlLiteral()
        case IDENTIFIER | BACKQUOTED_IDENT | THIS | SUPER => path(thisOK = true, typeOK = false)
        case USCORE                                       => freshPlaceholder()
        case LPAREN                                       => atPos(in.offset)(makeParens(commaSeparated(expr())))
        case LBRACE                                       => blockExpr()
        case NEW                                          => mkNew()
        case _                                            => syntaxErrorOrIncompleteAnd("illegal start of simple expression", skipIt = true)(errorTermTree)
      }
      simpleExprRest(t, canApply = canApply)
    }

    def simpleExprRest(t: Tree, canApply: Boolean): Tree = {
      if (canApply) newLineOptWhenFollowedBy(LBRACE)
      in.token match {
        case DOT =>
          in.nextToken()
          simpleExprRest(selector(stripParens(t)), canApply = true)
        case LBRACKET =>
          val t1 = stripParens(t)
          t1 match {
            case Ident(_) | Select(_, _) | Apply(_, _) =>
              var app: Tree = t1
              while (in.token == LBRACKET)
                app = atPos(app.pos.start, in.offset)(TypeApply(app, exprTypeArgs()))

              simpleExprRest(app, canApply = true)
            case _ =>
              t1
          }
        case LPAREN | LBRACE if (canApply) =>
          val app = atPos(t.pos.start, in.offset) {
            // look for anonymous function application like (f _)(x) and
            // translate to (f _).apply(x), bug #460
            val sel = t match {
              case Parens(List(Typed(_, _: Function))) =>
                Select(stripParens(t), nme.apply)
              case _ =>
                stripParens(t)
            }
            Apply(sel, argumentExprs())
          }
          simpleExprRest(app, canApply = true)
        case USCORE =>
          atPos(t.pos.start, in.skipToken()) {
            Typed(stripParens(t), Function(Nil, EmptyTree))
          }
        case _ =>
          t
      }
    }

    /** {{{
     *  ArgumentExprs ::= `(' [Exprs] `)'
     *                  | [nl] BlockExpr
     *  }}}
     */
    def argumentExprs(): List[Tree] = {
      def args(): List[Tree] = commaSeparated(
        if (isIdent) treeInfo.assignmentToMaybeNamedArg(expr()) else expr()
      )
      in.token match {
        case LBRACE   => List(blockExpr())
        case LPAREN   => inParens(if (in.token == RPAREN) Nil else args())
        case _        => Nil
      }
    }
    /** A succession of argument lists. */
    def multipleArgumentExprs(): List[List[Tree]] = {
      if (in.token != LPAREN) Nil
      else argumentExprs() :: multipleArgumentExprs()
    }

    /** {{{
     *  BlockExpr ::= `{' (CaseClauses | Block) `}'
     *  }}}
     */
    def blockExpr(): Tree = atPos(in.offset) {
      inBraces {
        if (isCaseDefStart) Match(EmptyTree, caseClauses())
        else block()
      }
    }

    /** {{{
     *  Block ::= BlockStatSeq
     *  }}}
     *  @note  Return tree does not carry position.
     */
    def block(): Tree = makeBlock(blockStatSeq())

    def caseClause(): CaseDef =
      atPos(in.offset)(makeCaseDef(pattern(), guard(), caseBlock()))

    /** {{{
     *  CaseClauses ::= CaseClause {CaseClause}
     *  CaseClause  ::= case Pattern [Guard] `=>' Block
     *  }}}
     */
    def caseClauses(): List[CaseDef] = {
      val cases = caseSeparated { caseClause() }
      if (cases.isEmpty)  // trigger error if there are no cases
        accept(CASE)

      cases
    }

    // IDE HOOK (so we can memoize case blocks) // needed?
    def caseBlock(): Tree =
      atPos(accept(ARROW))(block())

    /** {{{
     *  Guard ::= if PostfixExpr
     *  }}}
     */
    def guard(): Tree = if (acceptIfPresent(IF)) guardExpr() else EmptyTree

    def guardExpr(): Tree = stripParens(postfixExpr())

    /** {{{
     *  Enumerators ::= Generator {semi Enumerator}
     *  Enumerator  ::=  Generator
     *                |  Guard
     *                |  val Pattern1 `=' Expr
     *  }}}
     */
    def enumerators(): List[Enumerator] = {
      val enums = new ListBuffer[Enumerator]
      generator(enums, eqOK = false)
      while (isStatSep) {
        in.nextToken()
        if (in.token == IF) enums += makeFilter(in.offset, guard())
        else generator(enums, eqOK = true)
      }
      enums.toList
    }

    /** {{{
     *  Generator ::= Pattern1 (`<-' | `=') Expr [Guard]
     *  }}}
     */
    def generator(enums: ListBuffer[Enumerator], eqOK: Boolean) {
      val start      = in.offset
      val hasVal     = acceptIfPresent(VAL)
      val pat        = noSeq.pattern1()
      val point      = in.offset
      val equalsBody = equalsExprOpt()
      val hasEq      = !equalsBody.isEmpty

      if (hasVal && !hasEq)
        syntaxError(start, "val in for comprehension must be followed by assignment")
      else if (hasEq && !eqOK)
        syntaxError(point, "for comprehension must start with generator: " + expectedMsg(expected = LARROW, found = EQUALS))
      else if (hasVal)
        deprecationWarning(start, "val keyword in for comprehension is deprecated")

      val rhs = equalsBody orElse { accept(LARROW) ; expr() }

      enums += makeGenerator(r2p(start, point, in.lastOffset max start), pat, hasEq, rhs)
      // why max above? IDE stress tests have shown that lastOffset could be less than start,
      // I guess this happens if instead if a for-expression we sit on a closing paren.
      enums ++= tokenSeparated(IF, sepFirst = true, makeFilter(in.offset, guardExpr()))
    }

    def makeFilter(start: Int, tree: Tree) = Filter(r2p(start, tree.pos.point, tree.pos.end), tree)

/* -------- PATTERNS ------------------------------------------- */

    /** Methods which implicitly propagate whether the initial call took
     *  place in a context where sequences are allowed.  Formerly, this
     *  was threaded through methods as boolean seqOK.
     */
    trait SeqContextSensitive extends PatternContextSensitive {
      // is a sequence pattern _* allowed?
      def isSequenceOK: Boolean

      // are we in an XML pattern?
      def isXML: Boolean = false

      def functionArgType(): Tree = argType()
      def argType(): Tree = {
        val start = in.offset
        in.token match {
          case USCORE =>
            in.nextToken()
            if (in.token == SUBTYPE || in.token == SUPERTYPE) wildcardType(start)
            else atPos(start) { Bind(tpnme.WILDCARD, EmptyTree) }
          case IDENTIFIER if nme.isVariableName(in.name) =>
            atPos(start) { Bind(identForType(), EmptyTree) }
          case _ =>
            typ()
        }
      }

      /** {{{
       *  Patterns ::= Pattern { `,' Pattern }
       *  SeqPatterns ::= SeqPattern { `,' SeqPattern }
       *  }}}
       */
      def patterns(): List[Tree] = commaSeparated(pattern())

      /** {{{
       *  Pattern  ::=  Pattern1 { `|' Pattern1 }
       *  SeqPattern ::= SeqPattern1 { `|' SeqPattern1 }
       *  }}}
       */
      def pattern(): Tree = {
        val start = in.offset
        def loop(): List[Tree] = pattern1() :: {
          if (isRawBar) { in.nextToken() ; loop() }
          else Nil
        }
        loop() match {
          case pat :: Nil => pat
          case xs         => atPos(start)(makeAlternative(xs))
        }
      }

      /** {{{
       *  Pattern1    ::= varid `:' TypePat
       *                |  `_' `:' TypePat
       *                |  Pattern2
       *  SeqPattern1 ::= varid `:' TypePat
       *                |  `_' `:' TypePat
       *                |  [SeqPattern2]
       *  }}}
       */
      def pattern1(): Tree = pattern2() match {
        case p @ Ident(name) if in.token == COLON =>
          if (treeInfo.isVarPattern(p))
            atPos(p.pos.start, in.skipToken())(Typed(p, compoundType()))
          else {
            syntaxError("Pattern variables must start with a lower-case letter. (SLS 8.1.1.)")
            p
          }
        case p => p
      }

      /** {{{
       *  Pattern2    ::=  varid [ @ Pattern3 ]
       *                |   Pattern3
       *  SeqPattern2 ::=  varid [ @ SeqPattern3 ]
       *                |   SeqPattern3
       *  }}}
       */
      def pattern2(): Tree = pattern3() match {
        case Ident(nme.WILDCARD) if in.token == AT                          => in.nextToken() ; pattern3()
        case p @ Ident(name) if in.token == AT && (treeInfo isVarPattern p) => in.nextToken() ; atPos(p.pos.start)(Bind(name, pattern3()))
        case p                                                              => p
      }

      /** {{{
       *  Pattern3    ::= SimplePattern
       *                |  SimplePattern {Id [nl] SimplePattern}
       *  }}}
       */
      def pattern3(): Tree = {
        val top = simplePattern(badPattern3)
        val base = opstack
        // See SI-3189, SI-4832 for motivation. Cf SI-3480 for counter-motivation.
        // TODO: dredge out the remnants of regexp patterns.
        def peekaheadDelim(): Boolean = {
          peekahead()
          val result = in.token match {
            case RBRACE => isXML
            case RPAREN => !isXML
            case _      => false
          }
          result || { pushback() ; false }
        }
        def isWildStar = top match {
          case Ident(nme.WILDCARD) if isRawStar => peekaheadDelim()
          case _                                => false
        }
        def loop(top: Tree): Tree = reducePatternStack(base, top) match {
          case next if isIdentExcept(raw.BAR) => pushOpInfo(next) ; loop(simplePattern(badPattern3))
          case next                           => next
        }
        if (isSequenceOK && isWildStar)
          atPos(top.pos.start, in.prev.offset)(Star(stripParens(top)))
        else
          stripParens(loop(top))
      }

      def badPattern3(): Tree = {
        def isComma                = in.token == COMMA
        def isDelimeter            = in.token == RPAREN || in.token == RBRACE
        def isCommaOrDelimeter     = isComma || isDelimeter
        val (isUnderscore, isStar) = opstack match {
          case OpInfo(_, Ident(nme.WILDCARD), nme.STAR, _) :: _ => (true,   true)
          case OpInfo(_, _, nme.STAR, _) :: _                   => (false,  true)
          case _                                                => (false, false)
        }
        def isSeqPatternClose = isUnderscore && isStar && isSequenceOK && isDelimeter
        val msg = (isUnderscore, isStar, isSequenceOK) match {
          case (true, true, true) if isComma             => "bad use of _* (a sequence pattern must be the last pattern)"
          case (true, true, true) if isDelimeter         => "bad brace or paren after _*"
          case (true, true, false) if isDelimeter        => "bad use of _* (sequence pattern not allowed)"
          case (false, true, true) if isDelimeter        => "use _* to match a sequence"
          case (false, true, true) if isCommaOrDelimeter => "trailing * is not a valid pattern"
          case _                                         => "illegal start of simple pattern"
        }
        // better recovery if don't skip delims of patterns
        val skip = !isCommaOrDelimeter || isSeqPatternClose

        // println(s"badPattern3: in=${in.token} skip=$skip")
        syntaxErrorOrIncompleteAnd(msg, skip)(errorPatternTree)
      }

      /** {{{
       *  SimplePattern    ::= varid
       *                    |  `_'
       *                    |  literal
       *                    |  XmlPattern
       *                    |  StableId  /[TypeArgs]/ [`(' [Patterns] `)']
       *                    |  StableId  [`(' [Patterns] `)']
       *                    |  StableId  [`(' [Patterns] `,' [varid `@'] `_' `*' `)']
       *                    |  `(' [Patterns] `)'
       *  }}}
       *
       * XXX: Hook for IDE
       */
      def simplePattern(): Tree = (
        // simple diagnostics for this entry point
        simplePattern(() => syntaxErrorOrIncompleteAnd("illegal start of simple pattern", skipIt = true)(errorPatternTree))
      )
      def simplePattern(onError: () => Tree): Tree = {
        val start = in.offset
        in.token match {
          case IDENTIFIER | BACKQUOTED_IDENT | THIS =>
            val t = stableId()
            in.token match {
              case INTLIT | LONGLIT | FLOATLIT | DOUBLELIT =>
                t match {
                  case Ident(nme.MINUS) =>
                    return literal(isNegated = true, inPattern = true, start = start)
                  case _ =>
                }
              case _ =>
            }
            val typeAppliedTree = in.token match {
              case LBRACKET   => atPos(start, in.offset)(AppliedTypeTree(convertToTypeId(t), typeArgs()))
              case _          => t
            }
            in.token match {
              case LPAREN   => atPos(start, in.offset)(Apply(typeAppliedTree, argumentPatterns()))
              case _        => typeAppliedTree
            }
          case USCORE =>
            // in.nextToken()
            // atPos(start, start) { Ident(nme.WILDCARD) }
            atPos(in.skipToken())(Ident(nme.WILDCARD))
          case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT |
               STRINGLIT | INTERPOLATIONID | SYMBOLLIT | TRUE | FALSE | NULL =>
            literal(inPattern = true)
          case LPAREN =>
            atPos(start)(makeParens(noSeq.patterns()))
          case XMLSTART =>
            xmlLiteralPattern()
          case _ =>
            onError()
        }
      }
    }
    /** The implementation of the context sensitive methods for parsing outside of patterns. */
    object outPattern extends PatternContextSensitive {
      def argType(): Tree = typ()
      def functionArgType(): Tree = paramType(useStartAsPosition = true)
    }
    /** The implementation for parsing inside of patterns at points where sequences are allowed. */
    object seqOK extends SeqContextSensitive {
      val isSequenceOK = true
    }
    /** The implementation for parsing inside of patterns at points where sequences are disallowed. */
    object noSeq extends SeqContextSensitive {
      val isSequenceOK = false
    }
    /** For use from xml pattern, where sequence is allowed and encouraged. */
    object xmlSeqOK extends SeqContextSensitive {
      val isSequenceOK = true
      override val isXML = true
    }
    /** These are default entry points into the pattern context sensitive methods:
     *  they are all initiated from non-pattern context.
     */
    def typ(): Tree      = outPattern.typ()
    def startInfixType() = outPattern.infixType(InfixMode.FirstOp)
    def startAnnotType() = outPattern.annotType()
    def exprTypeArgs()   = outPattern.typeArgs()
    def exprSimpleType() = outPattern.simpleType()

    /** Default entry points into some pattern contexts. */
    def pattern(): Tree = noSeq.pattern()
    def seqPatterns(): List[Tree] = seqOK.patterns()
    def xmlSeqPatterns(): List[Tree] = xmlSeqOK.patterns() // Called from xml parser
    def argumentPatterns(): List[Tree] = inParens {
      if (in.token == RPAREN) Nil
      else seqPatterns()
    }
    def xmlLiteralPattern(): Tree

/* -------- MODIFIERS and ANNOTATIONS ------------------------------------------- */

    /** Drop `private` modifier when followed by a qualifier.
     *  Contract `abstract` and `override` to ABSOVERRIDE
     */
    private def normalizeModifers(mods: Modifiers): Modifiers =
      if (mods.isPrivate && mods.hasAccessBoundary)
        normalizeModifers(mods &~ Flags.PRIVATE)
      else if (mods hasAllFlags (Flags.ABSTRACT | Flags.OVERRIDE))
        normalizeModifers(mods &~ (Flags.ABSTRACT | Flags.OVERRIDE) | Flags.ABSOVERRIDE)
      else
        mods

    private def addMod(mods: Modifiers): Modifiers =
      addMod(mods, flagTokens(in.token), inTokenRange)

    private def addMod(mods: Modifiers, mod: Long, pos: Position): Modifiers = {
      if (mods hasFlag mod) syntaxError("repeated modifier")
      in.nextToken()
      (mods | mod) withPosition (mod, pos)
    }

    private def tokenRange(token: TokenData) =
      r2p(token.offset, token.offset, token.offset + token.name.length)

    /** {{{
     *  AccessQualifier ::= `[' (Id | this) `]'
     *  }}}
     */
    def accessQualifierOpt(mods: Modifiers): Modifiers = {
      def newModifiers(): Modifiers = (
        if (acceptIfPresent(THIS)) mods | Flags.LOCAL   // private/protected[this]
        else Modifiers(mods.flags, identForType())      // private/protected[foo]
      )
      in.token match {
        case LBRACKET if mods.hasAccessBoundary => syntaxError("duplicate private/protected qualifier") ; mods
        case LBRACKET                           => inBrackets(newModifiers())
        case _                                  => mods
      }
    }

    private val flagTokens: Map[Int, Long] = Map(
      ABSTRACT  -> Flags.ABSTRACT,
      FINAL     -> Flags.FINAL,
      IMPLICIT  -> Flags.IMPLICIT,
      LAZY      -> Flags.LAZY,
      OVERRIDE  -> Flags.OVERRIDE,
      PRIVATE   -> Flags.PRIVATE,
      PROTECTED -> Flags.PROTECTED,
      SEALED    -> Flags.SEALED
    )

    /** {{{
     *  AccessModifier ::= (private | protected) [AccessQualifier]
     *  }}}
     */
    def accessModifierOpt(): Modifiers = normalizeModifers {
      in.token match {
        case m @ (PRIVATE | PROTECTED)  => in.nextToken() ; accessQualifierOpt(Modifiers(flagTokens(m)))
        case _                          => NoMods
      }
    }

    /** {{{
     *  Modifiers ::= {Modifier}
     *  Modifier  ::= LocalModifier
     *              |  AccessModifier
     *              |  override
     *  }}}
     */
    def modifiers(): Modifiers = normalizeModifers {
      def loop(mods: Modifiers): Modifiers = in.token match {
        case PRIVATE | PROTECTED                                    => loop(accessQualifierOpt(addMod(mods)))
        case ABSTRACT | FINAL | SEALED | OVERRIDE | IMPLICIT | LAZY => loop(addMod(mods))
        case _                                                      => if (acceptIfPresent(NEWLINE)) loop(mods) else mods
      }
      loop(NoMods)
    }

    /** {{{
     *  LocalModifiers ::= {LocalModifier}
     *  LocalModifier  ::= abstract | final | sealed | implicit | lazy
     *  }}}
     */
    def localModifiers(): Modifiers = {
      def loop(mods: Modifiers): Modifiers = if (isLocalModifier) loop(addMod(mods)) else mods
      loop(NoMods)
    }

    /** {{{
     *  Annotations      ::= {`@' SimpleType {ArgumentExprs}}
     *  ConsrAnnotations ::= {`@' SimpleType ArgumentExprs}
     *  }}}
     */
    def annotations(skipNewLines: Boolean): List[Tree] = readAnnots {
      val t = annotationExpr()
      if (skipNewLines) newLineOpt()
      t
    }
    def constructorAnnotations(): List[Tree] = readAnnots {
      atPos(in.offset)(New(exprSimpleType(), List(argumentExprs())))
    }

    def annotationExpr(): Tree = atPos(in.offset) {
      val t = exprSimpleType()
      if (in.token == LPAREN) New(t, multipleArgumentExprs())
      else New(t, Nil)
    }

/* -------- PARAMETERS ------------------------------------------- */

    def allowTypelessParams = false

    /** {{{
     *  ParamClauses      ::= {ParamClause} [[nl] `(' implicit Params `)']
     *  ParamClause       ::= [nl] `(' [Params] `)'
     *  Params            ::= Param {`,' Param}
     *  Param             ::= {Annotation} Id [`:' ParamType] [`=' Expr]
     *  ClassParamClauses ::= {ClassParamClause} [[nl] `(' implicit ClassParams `)']
     *  ClassParamClause  ::= [nl] `(' [ClassParams] `)'
     *  ClassParams       ::= ClassParam {`,' ClassParam}
     *  ClassParam        ::= {Annotation}  [{Modifier} (`val' | `var')] Id [`:' ParamType] [`=' Expr]
     *  }}}
     */
    def paramClauses(owner: Name, contextBounds: List[Tree], ofCaseClass: Boolean): List[List[ValDef]] = {
      def inType   = owner.isTypeName
      val start    = in.offset
      val vparamss = new ListBuffer[List[ValDef]]
      var seenImplicit = false

      def typelessParamsOk = settings.YmethodInfer && !owner.isTypeName || allowTypelessParams
      def caseParam        = ofCaseClass && vparamss.isEmpty

      def param(): ValDef = {
        val start         = in.offset
        val annots        = annotations(skipNewLines = false)
        val mods          = if (inType) positionDefModifiers(modifiers() | Flags.PARAMACCESSOR) else modifiers() | Flags.PARAM
        val isVal         = acceptIfPresent(VAL)
        val isVar         = !isVal && acceptIfPresent(VAR)
        val isValOrVar    = isVal || isVar
        val paramAccessor = !isValOrVar
        val privateThis   = paramAccessor && !caseParam

        def checkByNameParam() {
          def word = if (mods.isMutable) "`var'" else "`val'"
          if (owner.isTypeName && isValOrVar)
            syntaxError(s"$word parameters may not be call-by-name")
          else if (seenImplicit )
            syntaxError("implicit parameters may not be call-by-name")
        }

        val nameOffset = in.offset
        val name       = identForTerm()
        def namePos    = if (name == nme.ERROR) start else nameOffset
        val hasTpt     = acceptIfPresent(COLON)
        val isByName   = hasTpt && (in.token == ARROW) && { checkByNameParam() ; true }

        val tpt = (
          if (hasTpt) paramType()
          else if (typelessParamsOk) TypeTree() // inferred method parameters, like def (p1, p2, p3) = p1 + p2 + p3
          else { accept(COLON) ; EmptyTree }
        )
        val rhs     = equalsExprOpt()
        val newMods = ( mods
          | ( if (isByName)      Flags.BYNAMEPARAM   else 0L )
          | ( if (!rhs.isEmpty)  Flags.DEFAULTPARAM  else 0L )
          | ( if (seenImplicit)  Flags.IMPLICIT      else 0L )
          | ( if (caseParam)     Flags.CASEACCESSOR  else 0L )
          | ( if (privateThis)   Flags.PrivateLocal  else 0L )
        )
        atPos(start, namePos)(ValDef(newMods withAnnotations annots, name, tpt, rhs))
      }
      def paramClause(): List[ValDef] = {
        accept(LPAREN)
        if (acceptIfPresent(RPAREN)) Nil
        else {
          seenImplicit ||= acceptIfPresent(IMPLICIT)
          try commaSeparated(param()) finally accept(RPAREN)
        }
      }

      newLineOptWhenFollowedBy(LPAREN)
      while (!seenImplicit && in.token == LPAREN) {
        vparamss += paramClause()
        newLineOptWhenFollowedBy(LPAREN)
      }
      val result = vparamss.toList
      def hasNoNonImplicitList = result match {
        case Nil              => true
        case (hd :: _) :: Nil => hd.mods.isImplicit
        case _                => false
      }
      if (owner == nme.CONSTRUCTOR && hasNoNonImplicitList) {
        in.token match {
          case LBRACKET   => syntaxError("no type parameters allowed here")
          case EOF        => incompleteInputError("auxiliary constructor needs non-implicit parameter list")
          case _          => syntaxError(start, "auxiliary constructor needs non-implicit parameter list")
        }
      }
      if (result.isEmpty && ofCaseClass) {
        syntaxError(in.lastOffset, "case classes without a parameter list are not allowed;\n"+
                                   "use either case objects or case classes with an explicit `()' as a parameter list.")
      }

      addEvidenceParams(owner, result, contextBounds)
    }

    /** {{{
     *  ParamType ::= Type | `=>' Type | Type `*'
     *  }}}
     */
    def paramType(): Tree = paramType(useStartAsPosition = false)
    def paramType(useStartAsPosition: Boolean): Tree = atPos(in.offset) {
      val isByName   = acceptIfPresent(ARROW)
      val tpt        = typ()
      val isRepeated = acceptIdentIfPresent(raw.STAR)
      def finish(): Tree = (
        if (isByName) byNameApplication(tpt)
        else if (isRepeated) repeatedApplication(tpt)
        else tpt
      )
      if (useStartAsPosition)
        finish()
      else
        atPos(tpt.pos.start, tpt.pos.point)(finish())
    }

    /** {{{
     *  TypeParamClauseOpt    ::= [TypeParamClause]
     *  TypeParamClause       ::= `[' VariantTypeParam {`,' VariantTypeParam} `]']
     *  VariantTypeParam      ::= {Annotation} [`+' | `-'] TypeParam
     *  FunTypeParamClauseOpt ::= [FunTypeParamClause]
     *  FunTypeParamClause    ::= `[' TypeParam {`,' TypeParam} `]']
     *  TypeParam             ::= Id TypeParamClauseOpt TypeBounds {<% Type} {":" Type}
     *  }}}
     */
    def typeParamClauseOpt(owner: Name, contextBoundBuf: ListBuffer[Tree]): List[TypeDef] = {
      def typeParam(ms: Modifiers): TypeDef = {
        var mods = ms | Flags.PARAM
        val start = in.offset
        if (owner.isTypeName && isIdent) {
          if (acceptIdentIfPresent(raw.PLUS))
            mods |= Flags.COVARIANT
          else if (acceptIdentIfPresent(raw.MINUS))
            mods |= Flags.CONTRAVARIANT
        }
        val nameOffset = in.offset
        // TODO AM: freshName(o2p(in.skipToken()), "_$$"), will need to update test suite
        val pname: TypeName = wildcardOrIdent().toTypeName
        val param = atPos(start, nameOffset) {
          val tparams = typeParamClauseOpt(pname, null) // @M TODO null --> no higher-order context bounds for now
          TypeDef(mods, pname, tparams, typeBounds())
        }
        if (contextBoundBuf ne null) {
          while (in.token == VIEWBOUND) {
            contextBoundBuf += atPos(in.skipToken())(makeFunctionTypeTree(List(Ident(pname)), typ()))
          }
          while (in.token == COLON) {
            val start   = in.skipToken()
            val tycon   = typ()
            val applied = atPos(tycon.pos withStart start)(AppliedTypeTree(tycon, Ident(pname) :: Nil))
            contextBoundBuf += applied
          }
        }
        param
      }
      newLineOptWhenFollowedBy(LBRACKET)
      if (in.token == LBRACKET) inBrackets(commaSeparated(typeParam(NoMods withAnnotations annotations(skipNewLines = true))))
      else Nil
    }

    /** {{{
     *  TypeBounds ::= [`>:' Type] [`<:' Type]
     *  }}}
     */
    def typeBounds(): TypeBoundsTree = {
      val lo      = bound(SUPERTYPE)
      val hi      = bound(SUBTYPE)
      val t       = TypeBoundsTree(lo, hi)
      val defined = List(t.hi, t.lo) filter (_.pos.isDefined)

      if (defined.nonEmpty)
        t setPos wrappingPos(defined)
      else
        t setPos o2p(in.offset)
    }

    def bound(tok: Int): Tree = if (acceptIfPresent(tok)) typ() else EmptyTree

/* -------- DEFS ------------------------------------------- */


    /** {{{
     *  Import  ::= import ImportExpr {`,' ImportExpr}
     *  }}}
     */
    def importClause(): List[Tree] = {
      val start = accept(IMPORT)
      commaSeparated(importExpr()) match {
        case Nil       => Nil
        case t :: rest =>
          // The first import should start at the position of the keyword.
          (t setPos (t.pos withStart start)) :: rest
      }
    }

    /** {{{
     *  ImportExpr ::= StableId `.' (Id | `_' | ImportSelectors)
     *  }}}
     */
    def importExpr(): Tree = {
      val start = in.offset
      def thisDotted(name: TypeName) = {
        in.nextToken()
        val t = atPos(start)(This(name))
        accept(DOT)
        val result = selector(t)
        accept(DOT)
        result
      }
      /* Walks down import `foo.bar.baz.{ ... }` until it ends at a
       * an underscore, a left brace, or an undotted identifier.
       */
      def loop(expr: Tree): Tree = {
        expr setPos expr.pos.makeTransparent
        val selectors: List[ImportSelector] = in.token match {
          case USCORE   => List(importSelector()) // import foo.bar._;
          case LBRACE   => importSelectors()      // import foo.bar.{ x, y, z }
          case _        =>
            val nameOffset = in.offset
            val name = ident()
            if (acceptIfPresent(DOT))
              // import foo.bar.ident.<unknown> and so create a select node and recurse.
              return loop(atPos(start, if (name == nme.ERROR) in.offset else nameOffset)(Select(expr, name)))
            // import foo.bar.Baz;
            else List(makeImportSelector(name, nameOffset))
        }
        // reaching here means we're done walking.
        atPos(start)(Import(expr, selectors))
      }

      loop(in.token match {
        case THIS   => thisDotted(tpnme.EMPTY)
        case _      =>
          val id = atPos(start)(Ident(ident()))
          accept(DOT)
          if (in.token == THIS) thisDotted(id.name.toTypeName)
          else id
      })
    }

    /** {{{
     *  ImportSelectors ::= `{' {ImportSelector `,'} (ImportSelector | `_') `}'
     *  }}}
     */
    def importSelectors(): List[ImportSelector] = {
      val selectors = inBracesOrNil(commaSeparated(importSelector()))
      selectors.init foreach {
        case ImportSelector(nme.WILDCARD, pos, _, _)  => syntaxError(pos, "Wildcard import must be in last position")
        case _                                        => ()
      }
      selectors
    }

    def wildcardOrIdent(): Name = if (acceptIfPresent(USCORE)) nme.WILDCARD else ident()

    /** {{{
     *  ImportSelector ::= Id [`=>' Id | `=>' `_']
     *  }}}
     */
    def importSelector(): ImportSelector = {
      val start = in.offset
      val name  = wildcardOrIdent()

      // The first case is overly cleverly using named arguments to reverse the
      // positions of the last two parameters, because reading the rename will
      // move the value of in.offset. Hey, I didn't invent side effects, I too am
      // a victim here. I can't find a single place where the rename position
      // is used anyway.
      if (acceptIfPresent(ARROW))
        ImportSelector(name, start, renamePos = in.offset, rename = wildcardOrIdent())
      else if (name == nme.WILDCARD)
        ImportSelector(name, start, null, -1)
      else
        ImportSelector(name, start, name, start)
    }

    /** {{{
     *  Def    ::= val PatDef
     *           | var PatDef
     *           | def FunDef
     *           | type [nl] TypeDef
     *           | TmplDef
     *  Dcl    ::= val PatDcl
     *           | var PatDcl
     *           | def FunDcl
     *           | type [nl] TypeDcl
     *  }}}
     */
    def defOrDcl(mods: Modifiers): List[Tree] = {
      val newMods = positionDefModifiers(mods)
      in.token match {
        case VAR | VAL => patDefOrDcl(newMods)
        case DEF       => funDefOrDcl(newMods) :: Nil
        case TYPE      => typeDefOrDcl(newMods) :: Nil
        case _         => tmplDef(newMods) :: Nil
      }
    }

    def positionDefModifiers(mods: Modifiers): Modifiers = {
      val key = in.token
      val extraFlags = key match {
        case CASECLASS | CASEOBJECT => Flags.CASE
        case TRAIT                  => Flags.TRAIT | Flags.ABSTRACT
        case VAR                    => Flags.MUTABLE
        case _                      => 0L
      }
      if (mods.isLazy) key match {
        case VAL if mods.isParamAccessor => syntaxError("lazy modifier not allowed here. Use call-by-name parameters instead")
        case CLASS | CASECLASS | TRAIT   => syntaxError("classes cannot be lazy")
        case key if key != VAL           => syntaxError("lazy not allowed here. Only vals can be lazy")
        case _                           =>
      }
      mods | extraFlags withPosition key -> inTokenRange
    }

    /** {{{
     *  TmplDef ::= [case] class ClassDef
     *            |  [case] object ObjectDef
     *            |  [override] trait TraitDef
     *  }}}
     */
    def tmplDef(mods: Modifiers): Tree = {
      val newMods = positionDefModifiers(mods)
      in.token match {
        case CLASS | CASECLASS | TRAIT => classDef(newMods)
        case OBJECT | CASEOBJECT       => objectDef(newMods)
        case _                         => syntaxErrorOrIncompleteAnd("expected start of definition", skipIt = true)(EmptyTree)
      }
    }

    private def inTokenRange = tokenRange(in.token match {
      case CASECLASS | CASEOBJECT => in.prev
      case _                      => in
    })

    def nonLocalDefOrDcl : List[Tree] = {
      val annots = annotations(skipNewLines = true)
      defOrDcl(modifiers() withAnnotations annots)
    }

    def equalsExprOpt(): Tree = if (acceptIfPresent(EQUALS)) expr() else EmptyTree

    /** {{{
     *  PatDef ::= Pattern2 {`,' Pattern2} [`:' Type] `=' Expr
     *  ValDcl ::= Id {`,' Id} `:' Type
     *  VarDef ::= PatDef | Id {`,' Id} `:' Type `=' `_'
     *  }}}
     */
    def patDefOrDcl(mods: Modifiers): List[Tree] = {
      /** In a situation like
       *    private val FOO, BAR, BAZ = Value
       *  Positions are tricky. This is expanded into three ValDefs.
       *  The range of the first one includes the val keyword and any
       *  modifiers. The range of the last one extends from its name
       *  through the end of the body. All the others have range
       *  positions which include only the name being defined.
       */
      val pos          = in.skipToken()
      val lhses        = commaSeparated(stripParens(noSeq.pattern2()))
      val lhs          = lhses.last
      val tpt          = typedOpt()
      val ascriptedLhs = if (tpt.isEmpty) lhs else atUnionPos(lhs, tpt)(Typed(lhs, tpt))
      val hasEq        = acceptIfPresent(EQUALS)
      // SI-7854 an underscore following the equals doesn't necessarily mean default initialization.
      val isDefaultInit = hasEq && in.token == USCORE && {
        peekahead()
        isStatSep || isStatSeqEnd || { pushback() ; false }
      }
      val rhs       = if (hasEq && !isDefaultInit) expr() else EmptyTree
      def allIdents = lhses forall (_.isInstanceOf[Ident])

      def defaultInitFlag(): Long = {
        if (!allIdents)
          syntaxError(lhs.pos, "pattern definition is ineligible for default initialization")
        else if (!mods.isMutable)
          syntaxError(lhs.pos, "only vars are eligible for default initialization")
        else if (tpt.isEmpty)
          syntaxError(lhs.pos, "an explicit type is required for default initialization")

        Flags.DEFAULTINIT
      }
      def deferredFlag(): Long = {
        if (mods.isLazy)
          syntaxError(lhs.pos, "lazy values may not be abstract")         // e.g. lazy val foo: Int
        else if (!allIdents)
          syntaxError(lhs.pos, "pattern definition may not be abstract")  // e.g. val Some(x)

        Flags.DEFERRED
      }
      val newmods = mods | (
        if (isDefaultInit) defaultInitFlag()
        else if (rhs.isEmpty) deferredFlag()
        else 0L
      )

      def makeValDefs(decl: Tree): List[Tree] = {
        val newTpt = if (tpt.isEmpty) decl else Typed(decl, tpt.duplicate setPos tpt.pos.focus) setPos decl.pos.focus
        makePatDef(newmods, newTpt, rhs.duplicate setPos rhs.pos.focus) match {
          case tree :: Nil => (tree setPos decl.pos) :: Nil
          case trees       => trees map (_ setPos decl.pos.focus)
        }
      }

      val trees = (lhses.init flatMap makeValDefs) ::: makePatDef(newmods, ascriptedLhs, rhs)
      ensureNonOverlapping(trees.last, trees.init)
      trees
    }

    /** {{{
     *  VarDef ::= PatDef
     *           | Id {`,' Id} `:' Type `=' `_'
     *  VarDcl ::= Id {`,' Id} `:' Type
     *  }}}
    def varDefOrDcl(mods: Modifiers): List[Tree] = {
      var newmods = mods | Flags.MUTABLE
      val lhs = new ListBuffer[(Int, Name)]
      do {
        in.nextToken()
        lhs += (in.offset, ident())
      } while (in.token == COMMA)
      val tp = typedOpt()
      val rhs = if (tp.isEmpty || in.token == EQUALS) {
        accept(EQUALS)
        if (!tp.isEmpty && in.token == USCORE) {
          in.nextToken()
          EmptyTree
        } else {
          expr()
        }
      } else {
        newmods = newmods | Flags.DEFERRED
        EmptyTree
      }
    }
     */

    /** {{{
     *  FunDef ::= FunSig [`:' Type] `=' [`macro'] Expr
     *          |  FunSig [nl] `{' Block `}'
     *          |  `this' ParamClause ParamClauses
     *                 (`=' ConstrExpr | [nl] ConstrBlock)
     *  FunDcl ::= FunSig [`:' Type]
     *  FunSig ::= id [FunTypeParamClause] ParamClauses
     *  }}}
     */
    def funDefOrDcl(mods: Modifiers): Tree = {
      val start = in.skipToken()
      if (in.token == THIS) {
        atPos(start, in.skipToken()) {
          val cbounds = classContextBounds map (_.duplicate)
          val vparamss = paramClauses(nme.CONSTRUCTOR, cbounds, ofCaseClass = false)
          newLineOptWhenFollowedBy(LBRACE)
          val rhs = in.token match {
            case LBRACE   => atPos(in.offset) { constrBlock(vparamss) }
            case _        => accept(EQUALS) ; atPos(in.offset) { constrExpr(vparamss) }
          }
          DefDef(mods, nme.CONSTRUCTOR, List(), vparamss, TypeTree(), rhs)
        }
      }
      else {
        val nameOffset = in.offset
        val name = ident()
        funDefRest(start, nameOffset, mods, name)
      }
    }

    def funDefRest(start: Int, nameOffset: Int, mods: Modifiers, name: Name): Tree = {
      val result = atPos(start, if (name.toTermName == nme.ERROR) start else nameOffset) {
        var newmods = mods
        // contextBoundBuf is for context bounded type parameters of the form
        // [T : B] or [T : => B]; it contains the equivalent implicit parameter type,
        // i.e. (B[T] or T => B)
        val contextBoundBuf = new ListBuffer[Tree]
        val tparams = typeParamClauseOpt(name, contextBoundBuf)
        val cbounds = contextBoundBuf.toList
        val vparamss = paramClauses(name, cbounds, ofCaseClass = false)
        newLineOptWhenFollowedBy(LBRACE)
        var restype = fromWithinReturnType(typedOpt())
        val rhs =
          if (isStatSep || in.token == RBRACE) {
            if (restype.isEmpty) restype = scalaUnitConstr
            newmods |= Flags.DEFERRED
            EmptyTree
          } else if (restype.isEmpty && in.token == LBRACE) {
            restype = scalaUnitConstr
            blockExpr()
          }
          else {
            if (in.token == EQUALS) {
              in.nextTokenAllow(nme.MACROkw)
              if (acceptIdentIfPresent(nme.MACROkw))
                newmods |= Flags.MACRO
            }
            else accept(EQUALS)

            expr()
          }

        DefDef(newmods, name.toTermName, tparams, vparamss, restype, rhs)
      }
      signalParseProgress(result.pos)
      result
    }

    /** {{{
     *  ConstrExpr      ::=  SelfInvocation
     *                    |  ConstrBlock
     *  }}}
     */
    def constrExpr(vparamss: List[List[ValDef]]): Tree =
      if (in.token == LBRACE) constrBlock(vparamss)
      else Block(selfInvocation(vparamss) :: Nil, literalUnit)

    /** {{{
     *  SelfInvocation  ::= this ArgumentExprs {ArgumentExprs}
     *  }}}
     */
    def selfInvocation(vparamss: List[List[ValDef]]): Tree =
      atPos(accept(THIS)) {
        newLineOptWhenFollowedBy(LBRACE)
        var t = Apply(Ident(nme.CONSTRUCTOR), argumentExprs())
        newLineOptWhenFollowedBy(LBRACE)
        while (in.token == LPAREN || in.token == LBRACE) {
          t = Apply(t, argumentExprs())
          newLineOptWhenFollowedBy(LBRACE)
        }
        if (classContextBounds.isEmpty) t
        else Apply(t, vparamss.last.map(vp => Ident(vp.name)))
      }

    /** {{{
     *  ConstrBlock    ::=  `{' SelfInvocation {semi BlockStat} `}'
     *  }}}
     */
    def constrBlock(vparamss: List[List[ValDef]]): Tree =
      atPos(in.skipToken()) {
        val stats = selfInvocation(vparamss) :: {
          if (isStatSep) { in.nextToken(); blockStatSeq() }
          else Nil
        }
        accept(RBRACE)
        Block(stats, literalUnit)
      }

    /** {{{
     *  TypeDef ::= type Id [TypeParamClause] `=' Type
     *            | FunSig `=' Expr
     *  TypeDcl ::= type Id [TypeParamClause] TypeBounds
     *  }}}
     */
    def typeDefOrDcl(mods: Modifiers): Tree = {
      val start = in.skipToken()
      newLinesOpt()
      atPos(start, in.offset) {
        val name = identForType()
        // @M! a type alias as well as an abstract type may declare type parameters
        val tparams = typeParamClauseOpt(name, null)
        in.token match {
          case EQUALS =>
            in.nextToken()
            TypeDef(mods, name, tparams, typ())
          case t if t == SUPERTYPE || t == SUBTYPE || t == COMMA || t == RBRACE || isStatSep(t) =>
            TypeDef(mods | Flags.DEFERRED, name, tparams, typeBounds())
          case _ =>
            syntaxErrorOrIncompleteAnd("`=', `>:', or `<:' expected", skipIt = true)(EmptyTree)
        }
      }
    }

    /** Hook for IDE, for top-level classes/objects. */
    def topLevelTmplDef: Tree = {
      val annots = annotations(skipNewLines = true)
      tmplDef(modifiers() withAnnotations annots)
    }

    /** {{{
     *  ClassDef ::= Id [TypeParamClause] {Annotation}
     *               [AccessModifier] ClassParamClauses RequiresTypeOpt ClassTemplateOpt
     *  TraitDef ::= Id [TypeParamClause] RequiresTypeOpt TraitTemplateOpt
     *  }}}
     */
    def classDef(mods: Modifiers): ClassDef = {
      val start      = in.skipToken()
      val nameOffset = in.offset
      val name       = identForType()
      val totalStart = (mods.pos union start).start

      atPos(totalStart, nameOffset) {
        savingClassContextBounds {
          val contextBoundBuf = new ListBuffer[Tree]
          val tparams = typeParamClauseOpt(name, contextBoundBuf)
          classContextBounds = contextBoundBuf.toList
          val tstart = (in.offset :: classContextBounds.map(_.pos.start)).min
          if (!classContextBounds.isEmpty && mods.isTrait) {
            syntaxError("traits cannot have type parameters with context bounds `: ...' nor view bounds `<% ...'")
            classContextBounds = List()
          }
          val constrAnnots = if (!mods.isTrait) constructorAnnotations() else Nil
          val (constrMods, vparamss) =
            if (mods.isTrait) (Modifiers(Flags.TRAIT), List())
            else (accessModifierOpt(), paramClauses(name, classContextBounds, ofCaseClass = mods.isCase))
          var mods1 = mods
          if (mods.isTrait) {
            if (settings.YvirtClasses && in.token == SUBTYPE) mods1 |= Flags.DEFERRED
          } else if (in.token == SUBTYPE) {
            syntaxError("classes are not allowed to be virtual")
          }
          val template = templateOpt(mods1, name, constrMods withAnnotations constrAnnots, vparamss, tstart)
          val result = gen.mkClassDef(mods1, name, tparams, template)
          // Context bounds generate implicit parameters (part of the template) with types
          // from tparams: we need to ensure these don't overlap
          if (classContextBounds.nonEmpty)
            ensureNonOverlapping(template, tparams)

          result
        }
      }
    }

    /** {{{
     *  ObjectDef       ::= Id ClassTemplateOpt
     *  }}}
     */
    def objectDef(mods: Modifiers): ModuleDef = objectDef(in.offset, mods)
    def objectDef(start: Int, mods: Modifiers): ModuleDef = {
      in.nextToken()
      val nameOffset = in.offset
      val name = ident()
      val tstart = in.offset
      atPos(start, if (name == nme.ERROR) start else nameOffset) {
        val mods1 = if (in.token == SUBTYPE) mods | Flags.DEFERRED else mods
        val template = templateOpt(mods1, name, NoMods, Nil, tstart)
        ModuleDef(mods1, name.toTermName, template)
      }
    }

    /** Create a tree representing a package object, converting
     *  {{{
     *    package object foo { ... }
     *  }}}
     *  to
     *  {{{
     *    package foo {
     *      object `package` { ... }
     *    }
     *  }}}
     */
    def packageObjectDef(start: Offset): PackageDef = {
      val defn   = objectDef(in.offset, NoMods)
      val module = copyModuleDef(defn)(name = nme.PACKAGEkw)
      val pid    = atPos(o2p(defn.pos.start))(Ident(defn.name))

      makePackaging(start, pid, module :: Nil)
    }
    def packageOrPackageObject(start: Offset): Tree = (
      if (in.token == OBJECT)
        joinComment(packageObjectDef(start) :: Nil).head
      else {
        in.flushDoc
        makePackaging(start, pkgQualId(), inBracesOrNil(topStatSeq()))
      }
    )
    // TODO - eliminate this and use "def packageObjectDef" (see call site of this
    // method for small elaboration.)
    def makePackageObject(start: Int, objDef: ModuleDef): PackageDef = objDef match {
      case ModuleDef(mods, name, impl) =>
        makePackaging(
          start, atPos(o2p(objDef.pos.start)){ Ident(name) }, List(ModuleDef(mods, nme.PACKAGEkw, impl)))
    }

    /** {{{
     *  ClassParents       ::= AnnotType {`(' [Exprs] `)'} {with AnnotType}
     *  TraitParents       ::= AnnotType {with AnnotType}
     *  }}}
     */
    def templateParents(): List[Tree] = {
      def readAppliedParent(): Tree = {
        val start = in.offset
        val parent = startAnnotType()
        in.token match {
          case LPAREN => atPos(start)((parent /: multipleArgumentExprs())(Apply.apply))
          case _      => parent
        }
      }
      tokenSeparated(WITH, sepFirst = false, readAppliedParent())
    }

    /** {{{
     *  ClassTemplate ::= [EarlyDefs with] ClassParents [TemplateBody]
     *  TraitTemplate ::= [EarlyDefs with] TraitParents [TemplateBody]
     *  EarlyDefs     ::= `{' [EarlyDef {semi EarlyDef}] `}'
     *  EarlyDef      ::= Annotations Modifiers PatDef
     *  }}}
     */
    def template(): (List[Tree], ValDef, List[Tree]) = {
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == LBRACE) {
        // @S: pre template body cannot stub like post body can!
        val (self, body) = templateBody(isPre = true)
        if (in.token == WITH && (self eq emptyValDef)) {
          val earlyDefs: List[Tree] = body.map(ensureEarlyDef).filter(_.nonEmpty)
          in.nextToken()
          val parents = templateParents()
          val (self1, body1) = templateBodyOpt(parenMeansSyntaxError = false)
          (parents, self1, earlyDefs ::: body1)
        } else {
          (List(), self, body)
        }
      } else {
        val parents = templateParents()
        val (self, body) = templateBodyOpt(parenMeansSyntaxError = false)
        (parents, self, body)
      }
    }

    def ensureEarlyDef(tree: Tree): Tree = tree match {
      case vdef @ ValDef(mods, _, _, _) if !mods.isDeferred =>
        copyValDef(vdef)(mods = mods | Flags.PRESUPER)
      case tdef @ TypeDef(mods, _, _, _) =>
        deprecationWarning(tdef.pos.point, "early type members are deprecated. Move them to the regular body: the semantics are the same.")
        copyTypeDef(tdef)(mods = mods | Flags.PRESUPER)
      case docdef @ DocDef(comm, rhs) =>
        treeCopy.DocDef(docdef, comm, rhs)
      case stat if !stat.isEmpty =>
        syntaxError(stat.pos, "only concrete field definitions allowed in early object initialization section")
        EmptyTree
      case _ =>
        EmptyTree
    }

    /** {{{
     *  ClassTemplateOpt ::= `extends' ClassTemplate | [[`extends'] TemplateBody]
     *  TraitTemplateOpt ::= TraitExtends TraitTemplate | [[`extends'] TemplateBody] | `<:' TemplateBody
     *  TraitExtends     ::= `extends' | `<:'
     *  }}}
     */
    def templateOpt(mods: Modifiers, name: Name, constrMods: Modifiers, vparamss: List[List[ValDef]], tstart: Int): Template = {
      val (parents, self, body) = (
        if (in.token == EXTENDS || in.token == SUBTYPE && mods.isTrait) {
          in.nextToken()
          template()
        }
        else {
          newLineOptWhenFollowedBy(LBRACE)
          val (self, body) = templateBodyOpt(parenMeansSyntaxError = mods.isTrait || name.isTermName)
          (List(), self, body)
        }
      )
      def anyvalConstructor() = (
        // Not a well-formed constructor, has to be finished later - see note
        // regarding AnyVal constructor in AddInterfaces.
        DefDef(NoMods, nme.CONSTRUCTOR, Nil, ListOfNil, TypeTree(), Block(Nil, literalUnit))
      )
      val parentPos = o2p(in.offset)
      val tstart1 = if (body.isEmpty && in.lastOffset < tstart) in.lastOffset else tstart
      atPos(tstart1) {
        // Exclude only the 9 primitives plus AnyVal.
        if (inScalaRootPackage && ScalaValueClassNames.contains(name))
          Template(parents, self, anyvalConstructor :: body)
        else
          gen.mkTemplate(gen.mkParents(mods, parents, parentPos),
                         self, constrMods, vparamss, body, o2p(tstart))
      }
    }

/* -------- TEMPLATES ------------------------------------------- */

    /** {{{
     *  TemplateBody ::= [nl] `{' TemplateStatSeq `}'
     *  }}}
     * @param isPre specifies whether in early initializer (true) or not (false)
     */
    def templateBody(isPre: Boolean) = inBraces(templateStatSeq(isPre = isPre)) match {
      case (self, Nil)  => (self, EmptyTree.asList)
      case result       => result
    }
    def templateBodyOpt(parenMeansSyntaxError: Boolean): (ValDef, List[Tree]) = {
      def empty = emptyValDef -> Nil
      newLineOptWhenFollowedBy(LBRACE)
      in.token match {
        case LBRACE                          => templateBody(isPre = false)
        case LPAREN if parenMeansSyntaxError => syntaxErrorAnd(s"traits or objects may not have parameters", skipIt = true)(empty)
        case LPAREN                          => abort("unexpected opening parenthesis")
        case _                               => empty
      }
    }

    /** {{{
     *  Refinement ::= [nl] `{' RefineStat {semi RefineStat} `}'
     *  }}}
     */
    def refinement(): List[Tree] = inBraces(refineStatSeq())

/* -------- STATSEQS ------------------------------------------- */

  /** Create a tree representing a packaging. */
    def makePackaging(start: Int, pkg: Tree, stats: List[Tree]): PackageDef = pkg match {
      case x: RefTree => atPos(start, pkg.pos.point)(PackageDef(x, stats))
    }

    def makeEmptyPackage(start: Int, stats: List[Tree]): PackageDef =
      makeEmptyPackage(o2p(start).focus, stats)

    def makeEmptyPackage(namePos: Position, stats: List[Tree]): PackageDef = {
      val pid        = Ident(nme.EMPTY_PACKAGE_NAME) setPos namePos
      val packagePos = stats.foldLeft(namePos)(_ union _.pos)
      atPos(packagePos)(PackageDef(pid, stats))
    }

    /** {{{
     *  TopStatSeq ::= TopStat {semi TopStat}
     *  TopStat ::= Annotations Modifiers TmplDef
     *            | Packaging
     *            | package object objectDef
     *            | Import
     *            |
     *  }}}
     */
    def topStatSeq(): List[Tree] = {
      val stats = new ListBuffer[Tree]
      while (!isStatSeqEnd) {
        stats ++= (in.token match {
          case PACKAGE  =>
            packageOrPackageObject(in.skipToken()) :: Nil
          case IMPORT =>
            in.flushDoc
            importClause()
          case x if isAnnotation || isTemplateIntro || isModifier =>
            joinComment(topLevelTmplDef :: Nil)
          case _ =>
            if (isStatSep) Nil
            else syntaxErrorOrIncompleteAnd("expected class or object definition", skipIt = true)(Nil)
        })
        acceptStatSepOpt()
      }
      stats.toList
    }

    /** {{{
     *  TemplateStatSeq  ::= [id [`:' Type] `=>'] TemplateStats
     *  }}}
     * @param isPre specifies whether in early initializer (true) or not (false)
     */
    def templateStatSeq(isPre : Boolean): (ValDef, List[Tree]) = checkNoEscapingPlaceholders {
      var self: ValDef = emptyValDef
      var firstOpt: Option[Tree] = None
      if (isExprIntro) {
        in.flushDoc
        val first = expr(InTemplate) // @S: first statement is potentially converted so cannot be stubbed.
        if (in.token == ARROW) {
          first match {
            case Typed(tree @ This(tpnme.EMPTY), tpt) =>
              self = atUnionPos(tree, tpt) { makeSelfDef(nme.WILDCARD, tpt) }
            case _ =>
              convertToParam(first) match {
                case tree @ ValDef(_, name, tpt, EmptyTree) if (name != nme.ERROR) =>
                  self = atUnionPos(tree, tpt) { makeSelfDef(name, tpt) }
                case _ =>
              }
          }
          in.nextToken()
        } else {
          firstOpt = Some(first)
          acceptStatSepOpt()
        }
      }
      (self, firstOpt ++: templateStats())
    }

    /** {{{
     *  TemplateStats    ::= TemplateStat {semi TemplateStat}
     *  TemplateStat     ::= Import
     *                     | Annotations Modifiers Def
     *                     | Annotations Modifiers Dcl
     *                     | Expr1
     *                     | super ArgumentExprs {ArgumentExprs}
     *                     |
     *  }}}
     */
    def templateStats(): List[Tree] = {
      val stats = new ListBuffer[Tree]
      while (!isStatSeqEnd) {
        if (in.token == IMPORT) {
          in.flushDoc
          stats ++= importClause()
        } else if (isDefIntro || isModifier || isAnnotation) {
          stats ++= joinComment(nonLocalDefOrDcl)
        } else if (isExprIntro) {
          in.flushDoc
          stats += statement(InTemplate)
        } else if (!isStatSep) {
          syntaxErrorOrIncomplete("illegal start of definition", skipIt = true)
        }
        acceptStatSepOpt()
      }
      stats.toList
    }

    /** Informal - for the repl and other direct parser accessors.
     */
    def templateStatsCompat(): List[Tree] = templateStats() match {
      case Nil => EmptyTree.asList
      case stats => stats
    }

    /** {{{
     *  RefineStatSeq    ::= RefineStat {semi RefineStat}
     *  RefineStat       ::= Dcl
     *                     | type TypeDef
     *                     |
     *  }}}
     */
    def refineStatSeq(): List[Tree] = checkNoEscapingPlaceholders {
      val stats = new ListBuffer[Tree]
      while (!isStatSeqEnd) {
        stats ++= refineStat()
        if (in.token != RBRACE) acceptStatSep()
      }
      stats.toList
    }

    def refineStat(): List[Tree] =
      if (isDclIntro) { // don't IDE hook
        joinComment(defOrDcl(NoMods))
      } else if (!isStatSep) {
        syntaxErrorOrIncomplete(
          "illegal start of declaration"+
          (if (inFunReturnType) " (possible cause: missing `=' in front of current method body)"
           else ""), skipIt = true)
        Nil
      } else Nil

    /** overridable IDE hook for local definitions of blockStatSeq
     *  Here's an idea how to fill in start and end positions.
    def localDef : List[Tree] = {
      atEndPos {
        atStartPos(in.offset) {
          val annots = annotations(skipNewLines = true)
          val mods = localModifiers() withAnnotations annots
          if (!(mods hasFlag ~(Flags.IMPLICIT | Flags.LAZY))) defOrDcl(mods)
          else List(tmplDef(mods))
        }
      } (in.offset)
    }
    */

    def localDef(isImplicit: Boolean): List[Tree] = {
      val implicitmod = if (isImplicit) Flags.IMPLICIT else 0L
      val annots      = annotations(skipNewLines = true)
      val mods        = localModifiers() | implicitmod withAnnotations annots
      val defs        =
        if (!(mods hasFlag ~(Flags.IMPLICIT | Flags.LAZY))) defOrDcl(mods)
        else tmplDef(mods) :: Nil

      in.token match {
        case RBRACE | CASE  => defs :+ setInPos(literalUnit)
        case _              => defs
      }
    }

    /** {{{
     *  BlockStatSeq ::= { BlockStat semi } [ResultExpr]
     *  BlockStat    ::= Import
     *                 | Annotations [implicit] [lazy] Def
     *                 | Annotations LocalModifiers TmplDef
     *                 | Expr1
     *                 |
     *  }}}
     */
    def blockStatSeq(): List[Tree] = checkNoEscapingPlaceholders {
      val stats = new ListBuffer[Tree]
      while (!isStatSeqEnd && !isCaseDefStart) {
        if (in.token == IMPORT) {
          stats ++= importClause()
          acceptStatSepOpt()
        }
        else if (isExprIntro) {
          stats += statement(InBlock)
          if (in.token != RBRACE && !isCaseDefStart) acceptStatSep()
        }
        else if (isDefIntro || isLocalModifier || isAnnotation) {
          val start = in.offset
          stats ++= (acceptIfPresent(IMPLICIT) match {
            case true if isIdent => implicitClosure(start, InBlock) :: Nil
            case isImplicit      => localDef(isImplicit)
          })
          acceptStatSepOpt()
        }
        else if (isStatSep) {
          in.nextToken()
        }
        else {
          val addendum = if (isModifier) " (no modifiers allowed here)" else ""
          syntaxErrorOrIncomplete(s"illegal start of statement$addendum", skipIt = true)
        }
      }
      stats.toList
    }

    /** {{{
     *  CompilationUnit ::= {package QualId semi} TopStatSeq
     *  }}}
     */
    def compilationUnit(): PackageDef = checkNoEscapingPlaceholders {
      val packageOffset = in.offset
      var nameOffset = packageOffset
      var nameEnd    = nameOffset
      def packageNamePos = r2p(packageOffset, nameOffset, nameEnd)

      def topstats(): List[Tree] = {
        val ts = new ListBuffer[Tree]
        while (acceptIfPresent(SEMI)) ()
        val start = in.offset
        if (acceptIfPresent(PACKAGE)) {
          nameOffset = in.offset
          if (in.token == OBJECT) {
            // TODO - this next line is supposed to be
            //    ts += packageObjectDef(start)
            // but this broke a scaladoc test (run/diagrams-filtering.scala) somehow.
            ts ++= joinComment(List(makePackageObject(start, objectDef(in.offset, NoMods))))
            if (in.token != EOF) {
              acceptStatSep()
              ts ++= topStatSeq()
            }
          } else {
            in.flushDoc
            val pkg = pkgQualId()
            nameEnd = in.lastOffset

            if (in.token == EOF) {
              ts += makePackaging(start, pkg, List())
            } else if (isStatSep) {
              in.nextToken()
              ts += makePackaging(start, pkg, topstats())
            } else {
              ts += inBraces(makePackaging(start, pkg, topStatSeq()))
              acceptStatSepOpt()
              ts ++= topStatSeq()
            }
          }
        } else {
          ts ++= topStatSeq()
        }
        ts.toList
      }

      val unitStart = in.offset
      resetPackage()
      topstats() match {
        case (stat @ PackageDef(_, _)) :: Nil => stat
        case stats                            => makeEmptyPackage(packageNamePos, stats)
      }
    }
  }
}
