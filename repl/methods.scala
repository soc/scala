@annotation.switch def escapedChar(ch: Char): String
def ->(str: String): (Int, ReplToken)
def ->(tok: ReplToken): (Int, ReplToken)
def <:<[U: Manifest](other: U)
def <<(): String
def >!(): Unit
def >#(): Unit
def >#[U](p: T => U): Unit
def >(): Unit
def >>!(implicit ord: Ordering[T]): Unit
def >>(implicit ord: Ordering[T]): Unit
def >?(p: T => Boolean): Unit
def >?(r: Regex): Unit
def >?(s: String): Unit
def ?
def ?[T: Manifest]
def ^?[U](pf: PartialFunction[T, U]): Seq[U]
def ^^[U](f: T => U): Seq[U]
def add(item: CharSequence): Unit = buf +
def addClasspath(arg: String): Unit
def addImports(ids: String*): IR.Result
def addLineToFile(item: CharSequence): Unit
def addReplay(cmd: String) = replayCommandStack ::
def afterTyper[T](op: => T): T
def aliasForType(path: String)
def aliasForType(path: String): Option[String]
def aliasNames
def aliases
def aliasesForPackage(pkg: String)
def alive
def allDefinedNames
def allImplicits
def allMembers
def allSeenTypes
def allSettings
def all[T](body: => T): Seq[T]
def alternativesFor(id: String): List[String]
def any2stringOf(x: Any, maxlen: Int)
def anyClass(x: Any): JClass               = if (x =
def anyRefMethodsToShow
def append(code: String, line: String): String
def apply(): InteractiveReader
def apply(): JLineHistory
def apply(): String
def apply(args: List[String])
def apply(contributors: List[T]): String = stringFromWriter { code
def apply(elem: Any): CompletionAware
def apply(forwardTo: () => Option[CompletionAware]): CompletionAware
def apply(id: Int): PhaseName = all find (_.id =
def apply(in: BufferedReader = defaultIn, out: JPrintWriter = defaultOut, interactive: Boolean = true): SimpleReader
def apply(intp: IMain): Power
def apply(line: String): ProcessResult
def apply(line: String): Result
def apply(lines: TraversableOnce[String])
def apply(map: collection.Map[String, CompletionAware]): CompletionAware
def apply(member: Tree)
def apply(name: String): Symbol
def apply(name: String, tpe: String, value: Any): NamedParam
def apply(repl: ILoop): Power
def apply(repl: ILoop, intp: IMain): Power
def apply(s: String): Parsed
def apply(s: String, cursor: Int): Parsed
def apply(s: String, cursor: Int, delimited: Char => Boolean): Parsed
def apply(sym: Symbol): Set[Symbol]
def apply(t: Throwable)
def apply(targs: Type*): Type
def apply(terms: () => List[String]): CompletionAware = apply(terms, _
def apply(terms: () => List[String], followFunction: String => Option[CompletionAware]): CompletionAware
def apply(tp: Type): TypeMemberCompletion
def apply(tp: Type, runtimeType: Type, param: NamedParam): TypeMemberCompletion
def apply(x: Node)
def applyRule[T](code: String, rule: UnitParser => T): T
def applyToResultMember[T](name: Name, f: Symbol => T)
def apply[T: Manifest](name: String, x: T): NamedParam
def apply[T: Manifest](x: T): NamedParam
def apply[T]()
def apply[T](body: => T)
def apply[T](body: => T): T
def args
def arityClasses
def asExpr
def asJavaList
def asList: List[JEntry]
def asModule
def asQualifiedImport
def asStrings
def asStrings
def atCurrent[T](body: => T): T
def atMap[T](phs: Seq[PhaseName])(body: => T): Seq[(PhaseName, T)]
def atMulti[T](phs: Seq[PhaseName])(body: => T): Seq[T]
def atPickler[T](op: => T): T
def at[T](ph: PhaseName)(body: => T): T
def await()
def banner
def beQuietDuring[T](body: => T): T
def beSilentDuring[T](operation: => T): T
def bind(name: String, boundType: String, value: Any): IR.Result
def bind(p: NamedParam): IR.Result
def bindError(t: Throwable)
def bindValue(name: String, x: Any): IR.Result
def bindValue(x: Any): IR.Result
def bind[T: Manifest](name: String, value: T): IR.Result
def braceList(tparams: List[String])
def breakIf[T: Manifest](assertion: => Boolean, args: NamedParam*): Unit
def break[T: Manifest](args: List[NamedParam]): Unit
def bts
def btsmap     = bts map (x
def bufferHead
def bufferTail
def bytes(): Array[Byte]
def call(name: String, args: Any*): AnyRef
def callEither(name: String, args: Any*): Either[Throwable, AnyRef]
def callOpt(name: String, args: Any*): Option[AnyRef]
def cancel()
def caseParamNamesForPath(path: String)
def caught()
def children
def chooseHandler(member: Tree): MemberHandler
def chooseReader(settings: Settings): InteractiveReader
def classLoader: AbstractFileClassLoader
def className
def classOfTerm(id: String): Option[JClass]
def clazz(name: String, x: Any): NamedParam
def clazz(x: Any): NamedParam
def clazz[T: Manifest]
def clean(str: String): String
def clear()
def clearExecutionWrapper() = _executionWrapper
def cmd(name: String, usage: String, help: String, f: String => Result): LoopCommand
def command(line: String): Result
def commands: List[LoopCommand]
def commonPrefix(xs: List[String]): String
def companion
def compare(s1: Symbol, s2: Symbol)
def compile(source: String): Boolean
def compileSources(sources: SourceFile*): Boolean
def compileSourcesKeepingRun(sources: SourceFile*)
def compileString(code: String): Boolean
def complete(_buf: String, cursor: Int, candidates: JList[CharSequence]): Int
def complete(buffer: String, cursor: Int): Candidates
def completer()
def completer(): ScalaCompleter
def completions
def completions(buf: String): List[String]
def completions(verbosity: Int)
def completionsFor(buffer: String): List[String]
def completionsFor(parsed: Parsed): List[String]
def context(code: String)
def count
def cozyLeft(other: ReplToken)
def cozyRight(other: ReplToken)
def createDefault(): InteractiveReader
def createImportForName(name: Name): String
def current
def current()         = if (index >
def currentArg
def currentChar
def currentLine
def currentPos
def debug(msg: => Any): Unit
def debugging[T](msg: String)(res: T)
def decl(code: String)
def decl(code: String)
def declares  = members filter (_.owner =
def default
def defaultFile: File
def defaultFileName
def defaultIn
def defaultLines
def defaultOut
def default[T]
def definedNames
def definedOrImported
def definedOrImported
def definedSymbols = prevRequests.toSet flatMap ((x: Request)
def definedTerms
def definedTypes
def definesImplicit
def definesTerm
def definesType
def definesValue
def definitionForName(name: Name): Option[MemberHandler]
def defn
def defn(code: String)
def defn(code: String)
def delimit(buffer: CharSequence, cursor: Int)
def delimited: Char
def deprecation: Boolean
def deprecation_=(x: Boolean)
def didGenerate(name: String)
def disambiguated(name: Name): String
def discarded
def dotted(s: String): Parsed
def dotted(s: String, cursor: Int): Parsed = new Parsed(onull(s), cursor, _ =
def droppedEnough() = unseenHistory.size >
def echoCommandMessage(msg: String): Unit
def effectiveTp
def empty: Completion
def empty: History
def enablePowerMode(isDuringInit: Boolean)
def enclMatch
def enclPre
def entries(): JListIterator[JEntry]
def entries(idx: Int): JListIterator[JEntry]
def eraseLine()
def erasure
def escape(text: String): String
def escapeChars: List[Char]
def eval
def evalPath
def evaluate()
def exclude(name: String): Boolean
def excludeEndsWith: List[String]
def excludeNames: List[String]
def excludeStartsWith: List[String]
def execute(id: String): Option[Any]
def execute(line: String)
def execute(line: String): Option[ExecResult]
def executionFor(buffer: String): Option[Path]
def executionFor(parsed: Parsed): Option[Any]
def executionWrapper
def expr(code: String)
def expr(code: String)
def extraCodeToEvaluate(req: Request): String
def fail(msg: String)
def failMsg = "Argument to :wrap must be the name of a method with signature [T](
def fileCompletion
def filterNotFunction(s: String): Boolean
def filtered(xs: List[String])
def firstIfDelimiter
def fixResRefs(code: String, line: String)
def flatName(id: String)
def flush(): Unit
def fn(): Boolean
def follow(id: String): Option[CompletionAware]
def foreach[U](f: Tree => U): Unit = t foreach { x
def freq[U](p: T => U)
def freshInternalVarName()
def freshUserVarName()
def fromClazz(clazz: JClass): String
def fromManifest[T: Manifest] : String
def fromTypedValue[T: Manifest](x: T): String
def fromValue(value: Any): String             = if (value =
def fullFlatName(name: String)
def fullPath(vname: Name): String
def fullPath(vname: String)
def functionString(tp: Type)
def generate: T
def generatedName(simpleName: String): Option[String]
def get
def get()
def get(idx: Int): CharSequence
def getCompilerClass(name: String)
def getCompilerModule(name: String)
def getEval: Option[AnyRef]
def getEvalTyped[T] : Option[T]
def getInterpreterClassLoader()
def getMulti
def getType(name: String, isModule: Boolean)
def glb[U: Manifest](other: U)
def goBack(num: Int): Unit
def grep(s: String)
def grep(s: String)
def handleTermRedefinition(name: TermName, old: Request, req: Request)
def handleTypeRedefinition(name: TypeName, old: Request, req: Request)
def hasAncestor(f: JClass => Boolean)
def hasAncestorInPackage(pkg: String)
def hasAncestorName(f: String => Boolean)
def hasLongHelp = _longHelp != null || longHelp !
def hasMethod(s: String) = methods exists (x => tos(x) =
def headLength
def height
def helpCommand(line: String): Result
def history
def id
def implicitSymbols
def implicitSymbols
def implicitSymbolsBySource: List[(Symbol, List[Symbol])]
def importHandlers                 = allHandlers collect { case x: ImportHandler
def importString
def imported(tp: Type)
def importedNames
def importedSymbols
def importedSymbols
def importedSymbolsBySource: List[(Symbol, List[Symbol])]
def importedTermNamed(name: String) = importedTermSymbols find (_.name.toString =
def importedTermSymbols    = importedSymbols collect { case x: TermSymbol
def importedTerms
def importedTypeSymbols    = importedSymbols collect { case x: TypeSymbol
def importedTypes
def importsImplicit
def impt(code: String)
def impt(code: String)
def impts(code: String)
def inImport = prevNonIdent =
def inPackage = owners find (x
def incomplete()
def indentCode(code: String)
def index
def index
def info
def info(msg: => Any): Unit
def info[T: Manifest]
def inherits  = members filterNot (_.owner =
def init
def init()
def init()
def initialize(postInitSignal: => Unit): Unit
def initializeSynchronous(): Unit
def insistsOnSpace
def interfaces: List[JClass]
def interpret(line: String): IR.Result
def interpret(line: String): Unit
def interpret(line: String, synthetic: Boolean): IR.Result
def interpretStartingWith(code: String): Option[String]
def interpreter
def interpreter_= (i: Interpreter): Unit = intp
def ires
def isAlphaId(t: ReplToken)
def isAsync
def isAtStart     = cursor <
def isConsecutiveTabs(buf: String, cursor: Int)
def isDefinedAt(t: Throwable)
def isDelimiter
def isDelimiter(buffer: CharSequence, cursor: Int)
def isDelimiterChar(ch: Char)
def isEmpty
def isEmpty
def isEmpty
def isEscapeChar(ch: Char): Boolean
def isEscaped
def isFinished()
def isFirstDelimiter
def isIdentPart(t: ReplToken)
def isIgnore(sym: Symbol)
def isInitializeComplete
def isInternalVarName(name: Name): Boolean
def isInternalVarName(name: String)
def isKeep(sym: Symbol)
def isLastDelimiter
def isLegalTopLevel
def isLineName(name: String)
def isNoImports
def isNoPredef
def isOperatorId(t: ReplToken)
def isParseable(line: String): Boolean
def isPredefImport
def isQualified
def isQuoteEnd(ch: Char): Boolean
def isQuoteStart(ch: Char): Boolean
def isQuoted
def isQuoted(s: String) = (s.length >= 2) && (s.head =
def isRecur(sym: Symbol)
def isReplDebug: Boolean
def isReplInfo: Boolean
def isReplPower: Boolean
def isReplTrace: Boolean
def isRuntimeTypeTighter
def isScalaAnonymous
def isShow
def isShowRaw
def isStripping
def isTruncating
def isUnqualified = args.size =
def isUserVarName(name: String)
def iterator: JIterator[JEntry]
def keepHandler(handler: MemberHandler)
def keyword
def languageSymbols
def languageWildcardHandlers
def languageWildcardSyms: List[Symbol]
def languageWildcards: List[Type]
def last
def lastIfDelimiter
def lastResult = Forwarder(()
def lastResultCompletion
def lastResultFor(parsed: Parsed)
def lastWarnings: List[(Position, String)]
def lastWarnings: List[(global.Position, String)]
def left
def line
def lineAfterTyper[T](op: => T): T
def lines
def load(): Unit
def loadAndRun: (String, Boolean)
def loadAndRunReq(req: Request)
def loadCommand(arg: String)
def loadFiles(settings: Settings)
def longHelp
def longestCommonPrefix(xs: List[String]): String
def looksLikeInvocation(code: String)
def looksLikePath(code: String) = (code !
def lookupTypeOf(name: Name)
def loop(todo: Set[Symbol]): Set[Symbol]
def loop(x: JClass): List[JClass]
def loop(xs: List[String]): List[KeyBinding]
def lub[U: Manifest](other: U)
def main(args: Array[String]): Unit
def main(settings: Settings): Unit
def man
def mapFunction(s: String)
def matchesContinue(line: String)
def matchesPrompt(line: String)
def maxSize: Int
def maxStringLength
def memberNamed(s: String) = members find (x => tos(x) =
def memberNames
def members
def members
def members(x: Symbol): List[Symbol]
def methodNames
def methodSignatureString(sym: Symbol)
def methodString()
def methodTypeToString(mt: MethodType)
def methods
def methods
def mkDotted
def mkUndelimited
def mods: Modifiers
def module
def moduleName
def moduleOf(name: String)
def module[T: Manifest]
def mostRecentLine
def mostRecentVar
def mostRecentVar: String
def moveTo(idx: Int)  = (idx > 0) && (idx <
def moveToEnd(): Unit
def moveToFirst()     = (size > 0) && (index !
def moveToLast()
def multi[T](body: => T): Seq[T] = multi map (ph
def name
def name: Name
def nameString
def name_ : Name
def neededHelp(): String
def next
def next()            = (index <
def noDotOrSlash = line forall (ch => ch != '.' && ch !
def normalizedType
def nullary(name: String, help: String, f: () => Result): LoopCommand
def onError(msg: String)
def onRunaway(line: Line[_]): Unit
def optCompilerClass(name: String)
def optCompilerModule(name: String)
def optFlatName(id: String)
def originalLine = if (_originalLine =
def owner
def owners
def p(x: Any)
def packageDecl
def packageNames
def packaged(code: String)
def packages
def paginate
def paginate_=(value: Boolean)
def paramNameString(sym: Symbol)
def paramString(sym: Symbol)
def paramsString(params: List[Symbol])
def parenList(params: List[Any])
def parse(bindings: String): List[KeyBinding]
def parse(line: String): Option[List[Tree]]
def parse(str: String): PhaseName
def pasteCommand(): Result
def pathTo(name: String)
def pathToName(name: Name): String
def pathToTerm(id: String): String
def pathToType(id: String): String
def phase
def pkg
def pkgName
def pkgmates
def pkgslurp
def position
def powerCmd(): Result
def pp(f: Seq[T] => Seq[T]): Unit
def ppfreq[U](p: T => U): Unit = freq(p) foreach { case (count, key)
def prettify(x: Any): TraversableOnce[String]
def prettify(x: String)
def prettify(x: T): TraversableOnce[String]
def prettify(xs: TraversableOnce[T]): TraversableOnce[String] = xs flatMap (x
def prettify[T](value: T): TraversableOnce[String]
def prettyName
def prettyPrint(code: String)
def prev
def prevNonIdent
def previous()
def print
def printColumns(items: List[String]): Unit
def printPath
def printToken(left: ReplToken, token: ReplToken)
def process(args: Array[String]): Boolean
def process(settings: Settings): Boolean
def processLine(line: String): Boolean
def processed
def prompt
def purge(): Unit
def quietBind(p: NamedParam): IR.Result
def quietImport(ids: String*): IR.Result
def quietRun[T](code: String)
def quietString(tp: String): String
def quieter(tpe: String, alsoStrip: String*): String
def quoteChars: List[(Char, Char)]
def rawString
def read
def readAssumingNo(prompt: String)
def readAssumingYes(prompt: String)
def readLine(prompt: String): String
def readOneKey(prompt: String)
def readOneKey(prompt: String)
def readOneLine()
def readOneLine(prompt: String)
def readOneLine(prompt: String): String
def readPath
def readYesOrNo(prompt: String, alt: => Boolean): Boolean
def reallyInterpret
def rebind(p: NamedParam): IR.Result
def redrawLine()
def redrawLineAndFlush(): Unit
def regularCompletion
def relativize(str: String): String
def relativize(sym: Symbol): String
def relativize(tp: Type): String
def replace(item: CharSequence): Unit
def replayCommands
def reqsToUse: List[ReqAndHandler]
def requestForIdent(line: String): Option[Request]
def requestForName(name: Name): Option[Request]
def requestHistoryForName(name: Name): List[Request]
def rerunForWarnings
def rerunWith(names: String*)
def res
def reset()
def reset(): Unit = x
def resetClassLoader()
def resetVerbosity()
def resetVerbosity() = verbosity
def resolvePathToSymbol(accessPath: String): Symbol
def restartSysCalls[R](body: => R, reset: => Unit): R
def resultExtractionCode(req: Request): String
def right
def run(code: String, sets: Settings = new Settings): String
def run(lines: List[String]): String
def runCompletion
def runForTranscript(code: String, settings: Settings): String
def runaway
def running
def running   = _state =
def runtimeClass: JClass
def runtimeClassAndTypeOfTerm(id: String): Option[(JClass, Type)]
def runtimeSymbol: Symbol
def runtimeType: Type
def runtimeTypeOfTerm(id: String): Option[Type]
def runtimeTypeString
def runtimeTypedParam
def safeClass(name: String): Option[Symbol]
def safeModule(name: String): Option[Symbol]
def sanitize(s: Array[Byte]): String
def sanitize(s: String): String
def savingSettings[T](fn: Settings => Unit)(body: => T): T
def scalaName(clazz: JClass): String
def scalaName(m: ClassManifest[_]): String
def scalaName(s: String): String
def scalaSigBytesForPath(path: String)
def scalaToJline(tc: ScalaCompleter): Completer
def select(reqs: List[ReqAndHandler], wanted: Set[Name]): List[ReqAndHandler]
def sessionImportedSymbols
def sessionWildcards: List[Type]
def set(phase: PhaseName): Boolean
def setContextClassLoader()
def setExecutionWrapper(code: String) = _executionWrapper
def setMulti(phases: Seq[PhaseName]): Boolean
def setPrompt(prompt: String) = currentPrompt
def set[T](code: String)(body: => T)
def settings
def shortClass
def show()
def show(x: Any): Unit
def show(x: String)
def show(x: T): Unit
def show(xs: TraversableOnce[T]): Unit
def showAt[T](phs: Seq[PhaseName])(body: => T): Unit
def showUsage(): Result
def show[T](body: => T): Seq[T]
def simpleNameOfType
def simpleNameOfType(name: TypeName)
def simpleParse(code: String): Tree
def size
def size
def skipArity(name: String) = arityClasses exists (x => name !
def slurp()
def slurp(): String
def slurp(): String
def sortFunction(s1: String, s2: String): Boolean
def source(code: String)
def spaces(code: String): String
def state
def staticTypeString
def staticTypedParam
def stmt(code: String)
def stmt(code: String)
def stmts(code: String)
def stmts(code: String)  = try (self.stmts(code) map (x
def string2code(str: String): String
def string2codeQuoted(str: String)
def stringOf(x: Any): String
def strings(s: Seq[Byte]): List[String]
def strip(str: String): String
def stripImpl(str: String): String
def stripString(s: String)
def success   = _state =
def superNames: List[String]
def supermans: List[Manifest[_]]
def supers: List[JClass]
def symbolDefString(sym: Symbol)
def symbolOfTerm(id: String): Symbol
def symbol_ : Symbol
def tapDebug(msg: => String): T = tap(x
def tapInfo(msg: => String): T  = tap(x
def tapTrace(msg: => String): T = tap(x
def tap[U](f: T => U): T
def targetType
def termNames
def terminal
def terms(name: String): Symbol
def this()
def this(in0: BufferedReader, out: JPrintWriter)
def this(settings: Settings)
def this(str: String)
def thread
def toCompute
def toJLine(args: List[String], cursor: Int)
def toManifest: Manifest[T]
def toTypeString: String
def tokenIterator: Iterator[ReplToken]
def tokens(code: String)
def tokens(line: String)
def topLevel
def topLevelFor(parsed: Parsed): List[String]
def topLevelThreshold
def tos(sym: Symbol)
def tparamsString(tparams: List[Symbol])
def tpe_ : Type
def trace(msg: => Any): Unit
def transcript(start: String)
def trees(code: String)
def truncate(str: String): String
def tryAll
def tryCompletion(p: Parsed, completionFunction: Parsed => List[String]): Option[Candidates]
def tupleString(tp: Type)
def typeCleanser(sym: Symbol, memberName: Name): Type
def typeNames
def typeOf(id: String): Type
def typeOf(name: String)
def typeOfExpression(expr: String, silent: Boolean = true): Option[Type]
def typeOfTerm(id: String): Option[Type]
def typeToString(tp: Type): String
def types
def types(name: String): Symbol
def u: URL
def unapply(that: Any): Option[CompletionAware]
def unapply(x: Cozy)
def undelimited(s: String, cursor: Int): Parsed = new Parsed(onull(s), cursor, _
def uniqueTags = children groupBy (_.label) filter (_._2.size =
def unit(code: String)
def unleash(): Unit
def unmangle(str: String): String
def unqualifiedIds
def url(s: String)
def usage: String
def usageMsg: String
def valueOfTerm(id: String): Option[AnyRef]
def varargs(name: String, usage: String, help: String, f: List[String] => Result): LoopCommand
def verbosity
def verbosity()
def visibleTermNames: List[Name]
def whoHas(name: String) = bts filter (_.decls exists (_.name.toString =
def width
def wildcardTypes
def withFile(filename: String)(action: File
def withLongHelp(text: String): this.type = { _longHelp
def withOriginalLine(s: String): this.type = { _originalLine
def withRawTokens[T](body: => T): T
def withVerbosity(v: Int): this.type = returning[this.type](this)(_ => _verbosity
def withoutSaving[T](op: => T): T
def withoutUnwrapping(op: => Unit): Unit
def words(s: String) = s.trim split "\\s+" filterNot (_ =
def |[U](f: Seq[T] => Seq[U]): Seq[U]
final def <--?-->(other: ReplToken)
final def defaultHelp
final def propOr(name: String): String
final def propOr(name: String, default: String): String
implicit def apply(s: String): PhaseName
implicit def apply[T: Manifest] : InternalInfo[T]
implicit def defaultPhaseName: PhaseName
implicit def liftToken(code: Int): Arrow
implicit def loopToInterpreter(repl: ILoop): IMain
implicit def namedValue[T: Manifest](name: String, x: T): NamedParam
implicit def phaseEnumToPhase(name: PhaseName): Phase
implicit def phaseNameToPhase(name: String): Phase
implicit def processResultToOutputLines(pr: ProcessResult): List[String]
implicit def replEnhancedStrings(s: String): RichReplString
implicit def replEnhancedURLs(url: URL)(implicit codec: Codec): RichReplURL
implicit def replInputStream(in: InputStream)(implicit codec: Codec)
implicit def replInternalInfo[T: Manifest](x: T): InternalInfo[T]
implicit def replMultiPrinting[T: Prettifier](xs: TraversableOnce[T]): MultiPrettifierClass[T]
implicit def replPrettifier[T] : Prettifier[T]
implicit def replPrinting[T](x: T)(implicit pretty: Prettifier[T] = Prettifier.default[T])
implicit def replTypeApplication(sym: Symbol): RichSymbol
implicit def resultFromString(msg: String): Result
implicit def resultFromUnit(x: Unit): Result
implicit def tuple[T: Manifest](pair: (String, T)): NamedParam
implicit def upDependentName(x: AnyName): Name
implicit def upDependentSymbol(x: AnySymbol): Symbol
implicit def upDependentTree(x: AnyTree): Tree
implicit def upDependentType(x: AnyType): Type
