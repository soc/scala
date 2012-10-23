case object    <*--|  extends Cozy(true, false)
case object    <*>    extends Cozy(true, true)
case object  |--*--|  extends Cozy(false, false)
case object  |--*>    extends Cozy(false, true)
case object At extends ReplToken("@") with InsistCozyRight
case object Cancelled extends State
case object Cleanup extends PhaseName
case object Closelim extends PhaseName
case object Colon extends ReplToken(":", <*--|)
case object Comma extends ReplToken(",", <*--|) with InsistCozyLeft
case object Constructors extends PhaseName
case object Dce extends PhaseName
case object Done extends State
case object Dot extends ReplToken(".", <*>) with InsistCozy
case object Eof extends ReplToken("EOF")
case object Erasure extends PhaseName
case object Error extends Result
case object ErrorToken extends ReplToken("<internal error>")
case object Explicitouter extends PhaseName
case object False extends ReplToken("false")
case object Flatten extends PhaseName
case object Icode extends PhaseName
case object Import extends ReplToken("import")
case object Incomplete extends Result
case object Inliner extends PhaseName
case object Jvm extends PhaseName
case object LBrace extends ReplToken
case object LBracket extends ReplToken("[") with Brackets
case object LParen extends ReplToken("(", |--*>)
case object Lambdalift extends PhaseName
case object Lazyvals extends PhaseName
case object Liftcode extends PhaseName
case object Mixin extends PhaseName
case object Namer extends PhaseName
case object Newline extends ReplToken("\n", <*>) with InsistCozy
case object Newlines extends ReplToken("\n\n", <*>) with InsistCozy
case object NoPhaseName extends PhaseName
case object Null extends ReplToken("null")
case object Packageobjects extends PhaseName
case object Parser extends PhaseName
case object Pickler extends PhaseName
case object RBrace extends ReplToken("}", |--*>) with InsistSpaced
case object RBracket extends ReplToken("]") with Brackets
case object RParen extends ReplToken(")", <*--|)
case object Refchecks extends PhaseName
case object Running extends State
case object Selectiveanf extends PhaseName
case object Selectivecps extends PhaseName
case object Semi extends ReplToken(";", <*--|)
case object Specialize extends PhaseName
case object Subtype extends ReplToken("<:") with InsistSpaced
case object Success extends Result
case object Superaccessors extends PhaseName
case object Supertype extends ReplToken(">:") with InsistSpaced
case object Tailcalls extends PhaseName
case object Terminal extends PhaseName
case object Threw extends State
case object True extends ReplToken("true")
case object Typer extends PhaseName
case object Uncurry extends PhaseName
case object ViewBound extends ReplToken("<%") with InsistSpaced
implicit object AnyPrettifier extends Prettifier[Any]
object AbstractOrMissingHandler
object ByteCode
object CodeHandlers
object CodeIncomplete extends CodeException("CodeIncomplete")
object Completion
object CompletionAware
object Cozy
object FileBackedHistory
object FileCompletion
object Forwarder
object History
object ILoop
object IMain
object Implicits extends Implicits2
object InteractiveReader
object InternalInfo extends LowPriorityInternalInfo
object JLineHistory
object KeyBinding
object Line
object LoopCommand
object NamedParam extends NamedParamCreator
object NoCompletion extends Completion
object NoHistory extends History
object NullCompleter extends ScalaCompleter
object Parsed
object PhaseName
object Power
object Prettifier extends LowPriorityPrettifier
object ProcessResult
object ProductCompletion
object Result
object Results
object Runner
object SimpleReader
object StringPrettifier extends Prettifier[String]
object TypeMemberCompletion
object TypeStrings extends TypeStrings
object XMLCompletion
object anyref extends TypeMemberCompletion(AnyRefClass.tpe)
object codeParser extends
object ids extends CompletionAware
object javalang extends PackageCompletion(JavaLangPackage.tpe)
object literals extends CompletionAware
object naming extends
object opt extends CodeHandlers[Option[T]]
object predef extends TypeMemberCompletion(PredefModule.tpe)
object replTokens extends
object rootClass extends TypeMemberCompletion(RootClass.tpe)
object scalalang extends PackageCompletion(ScalaPackage.tpe)
object symbolSubtypeOrdering extends Ordering[Symbol]
