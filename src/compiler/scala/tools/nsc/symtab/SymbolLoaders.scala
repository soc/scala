/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package symtab

import java.io.IOException
import scala.compat.Platform.currentTime
import scala.tools.nsc.util.{ ClassPath }
import classfile.ClassfileParser
import scala.reflect.internal.MissingRequirementError
import scala.reflect.internal.util.Statistics
import scala.reflect.io.{ AbstractFile, NoAbstractFile }

/** This class ...
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
abstract class SymbolLoaders {
  val global: Global
  import global._
  import SymbolLoadersStats._

  private var saved = 0
  sys addShutdownHook println(s"Saved $saved symbols")

  protected def enterIfNew(owner: Symbol, member: Symbol, completer: SymbolLoader): Symbol = {
    assert(owner.info.decls.lookup(member.name) == NoSymbol, owner.fullName + "." + member.name)
    owner.info.decls enter member
    member
  }

  protected def signalError(root: Symbol, ex: Throwable) {
    if (settings.debug) ex.printStackTrace()
    globalError(ex.getMessage() match {
      case null => "i/o error while loading " + root.name
      case msg  => "error while loading " + root.name + ", " + msg
    })
  }

  /** Enter class with given `name` into scope of `root`
   *  and give them `completer` as type.
   */
  def enterClass(owner: Symbol, name: String, completer: SymbolLoader): Symbol = {
    val clazz = owner.newClass(newTypeName(name))
    clazz setInfo completer
    enterIfNew(owner, clazz, completer)
  }

  /** Enter module with given `name` into scope of `root`
   *  and give them `completer` as type.
   */
  def enterModule(owner: Symbol, name: String, completer: SymbolLoader): Symbol = {
    val module = owner.newModule(newTermName(name))
    module setInfo completer
    module.moduleClass setInfo moduleClassLoader
    enterIfNew(owner, module, completer)
  }

  /** Enter package with given `name` into scope of `root`
   *  and give them `completer` as type.
   */
  def enterPackage(root: Symbol, name: String, completer: SymbolLoader): Symbol = {
    val pname = newTermName(name)
    val preExisting = root.info.decls lookup pname
    if (preExisting != NoSymbol) {
      // Some jars (often, obfuscated ones) include a package and
      // object with the same name. Rather than render them unusable,
      // offer a setting to resolve the conflict one way or the other.
      // This was motivated by the desire to use YourKit probes, which
      // require yjp.jar at runtime. See SI-2089.
      if (settings.termConflict.isDefault)
        throw new TypeError(
          root+" contains object and package with same name: "+
          name+"\none of them needs to be removed from classpath"
        )
      else if (settings.termConflict.value == "package") {
        global.warning(
          "Resolving package/object name conflict in favor of package " +
          preExisting.fullName + ".  The object will be inaccessible."
        )
        root.info.decls.unlink(preExisting)
      }
      else {
        global.warning(
          "Resolving package/object name conflict in favor of object " +
          preExisting.fullName + ".  The package will be inaccessible."
        )
        return NoSymbol
      }
    }
    // todo: find out initialization sequence for pkg/pkg.moduleClass is different from enterModule
    val pkg = root.newPackage(pname)
    pkg.moduleClass setInfo completer
    pkg setInfo pkg.moduleClass.tpe
    root.info.decls enter pkg
    pkg
  }

  private def sym_s(s: Symbol) = s"${s.accurateKindString} ${s.fullName}#${s.id} (${s.shortSymbolClass})"
  private def showme(label: Any, owner: Symbol, name: String) {
    val names = List(name, name + "$") flatMap (n => List(n: TermName, n: TypeName))
    val pairs = names map (n => (n.longString, owner.info member n)) filterNot (_._2 eq NoSymbol)
    val str   = pairs.map({ case (k, v) => f"$k%15s -> ${sym_s(v)}" }).mkString("\n        ")
    log(s"$label) in $owner I see\n        $str")
  }

  private def coordinateClassEntry(root: Symbol, name: String, completer: SymbolLoader) {
    val className  = TypeName( if (name endsWith "$") name.init else name )
    val moduleName = className.toTermName
    val mclassName = TypeName( if (name endsWith "$") name else name + "$" )

    val existingClass  = root.info.decls lookup className
    val existingModule = root.info.decls lookup moduleName
    val existingMClass = root.info.decls lookup mclassName

    def check(label: String, from: Symbol, op: Symbol => Symbol, expected: Symbol) {
      val outcome = op(from)
      if (outcome eq expected) return
      def from_s     = sym_s(from)
      def outcome_s  = sym_s(outcome)
      def expected_s = sym_s(expected)
      log(s"Inconsistent $label: arrow from $from_s goes to $outcome_s instead of $expected_s")
    }

    if (existingClass ne NoSymbol) {
      check("companionSymbol", existingClass, _.companionSymbol, existingModule)
      check("linkedClassOfClass", existingClass, _.linkedClassOfClass, existingMClass)
    }
    if (existingModule ne NoSymbol) {
      check("companionSymbol", existingModule, _.companionSymbol, existingClass)
      check("moduleClass", existingModule, _.moduleClass, existingMClass)
    }
    if (existingMClass ne NoSymbol) {
      check("linkedClassOfClass", existingMClass, _.linkedClassOfClass, existingClass)
      check("sourceModule", existingMClass, _.sourceModule, existingModule)
    }
  }

  private def enterViaModuleClass(root: Symbol, name: String, completer: SymbolLoader) {
    require(name endsWith "$", name)
    // showme("1", root, name.init)
    coordinateClassEntry(root, name, completer)
    val clazz = root.info.decls lookup (name: TypeName) orElse enterClass(root, name, completer)
    coordinateClassEntry(root, name, completer)

    // showme("2", root, name.init)
    // clazz match {
    //   case m: ModuleClassSymbol =>
    //     log(s"Leaving ModuleClassSymbol ${sym_s(m)} for $root")
    //   case mclass: ClassSymbol  =>
    //     log(s"(Not) Unlinking non-module ClassSymbol ${sym_s(mclass)} for $root")
    //     // root.info.decls unlink mclass
    //     // root.info.decls enter mclass.cloneSymbol(mclass.owner, mclass.flags | Flags.MODULE)
    //   case _ =>
    // }
    val module = root.info.decls lookup (name.init: TermName) orElse enterModule(root, name.init, completer)
    // showme("3", root, name.init)
    coordinateClassEntry(root, name, completer)
    println("")
  }

  // private def enterPackageClassAndObject(root: Symbol, name: String, completer: SymbolLoader) {
  //   log(s"enterPackageClassAndObject($root, $name, $completer)")
  //   showme(1)
  //   val clazz = root.info.decls lookup (name: TypeName) orElse enterClass(root, name, completer)
  //   showme(2)
  //   if (name endsWith "$") clazz match {
  //     case m: ModuleClassSymbol =>
  //       log(s"Leaving ModuleClassSymbol ${sym_s(m)} for $root")
  //     case mclass: ClassSymbol  =>
  //       log(s"Unlinking non-module ClassSymbol ${sym_s(mclass)} for $root")
  //       root.info.decls unlink mclass
  //     case _ =>
  //   }
  //   val module = (
  //     root.info.decls lookup (name: TermName) orElse enterModule(root, name, completer)
  //   )
  //   showme(3)
  //   println("")
  // }

  /** Enter class and module with given `name` into scope of `root`
   *  and give them `completer` as type.
   */
  def enterClassAndModule(root: Symbol, name: String, completer: SymbolLoader) {
    log(s"enterClassAndModule(${sym_s(root)}, $name, $completer)")

    val isModClass = name endsWith "$"
    if (isModClass)
      return enterViaModuleClass(root, name, completer)

    // if (name == "package")
    //   return enterPackageClassAndObject(root, name, completer)

    val modClass = root.info.decls lookup newTypeName(name + "$")
    val clazz    = enterClass(root, name, completer)
    val module: Symbol = modClass match {
      case mclass: ModuleClassSymbol =>
        val module = root.newModuleSymbol(newTermName(name), NoPosition, Flags.MODULE)
        println(s"Exciting: clazz=$clazz module=$module modClass=$modClass")
        connectModuleToClass(module, mclass)
        module
      case mclass: ClassSymbol =>
        root.info.decls unlink mclass
        val module   = root.info.decls lookup (name: TermName) orElse enterModule(root, name, completer)
        val modClass = module.moduleClass
        // class, module, moduleClass
        val strings  = List(clazz, module, modClass) map (s => s"${s.shortSymbolClass}#${s.id}") mkString ", "
        val classIs  = List(clazz, module.companionSymbol, modClass.linkedClassOfClass)
        val moduleIs = List(clazz.companionSymbol, module, modClass.sourceModule)
        val mclassIs = List(clazz.linkedClassOfClass, module.moduleClass, modClass)
        val ownerIs  = List(clazz.owner, module.owner, modClass.owner)
        def pairs = List(
          "class"        -> classIs,
          "module"       -> moduleIs,
          "module class" -> mclassIs,
          "owner"        -> ownerIs
        )

        if (List(classIs, moduleIs, mclassIs, ownerIs) forall (_.distinct.size == 1)) () else {
          log(s"Inconsistent symbol group: $strings in $root")
          log(List(clazz, module, modClass).map(s => s"${s.id}=${root.info.decls.lookup(s.name).alternatives.contains(s)}").mkString(s"In $root's info? ", ", ", ""))
          pairs foreach {
            case (what, List(classThinks, moduleThinks, mclassThinks)) =>
              if (classThinks != moduleThinks)
                log(s"The class thinks the $what is ${sym_s(classThinks)}, but the module says ${sym_s(moduleThinks)}")
              if (classThinks != mclassThinks)
                log(s"The class thinks the $what is ${sym_s(classThinks)}, but the module class says ${sym_s(mclassThinks)}")
              if (mclassThinks != moduleThinks)
                log(s"The module class thinks the $what is ${sym_s(mclassThinks)}, but the module says ${sym_s(moduleThinks)}")
          }
        }

        module
      case _ =>
        enterModule(root, name, completer)
    }

    if (!clazz.isAnonymousClass) {
      // Diagnostic for SI-7147
      def msg: String = {
        def symLocation(sym: Symbol) = if (sym == null) "null" else s"${clazz.fullLocationString} (from ${clazz.associatedFile})"
        sm"""Inconsistent class/module symbol pair for `$name` loaded from ${symLocation(root)}.
            |clazz = ${symLocation(clazz)}; clazz.companionModule = ${clazz.companionModule}
            |module = ${symLocation(module)}; module.companionClass = ${module.companionClass}"""
      }
      assert(clazz.companionModule == module, msg)
      assert(module.companionClass == clazz, msg)
    }
  }

  /** In batch mode: Enter class and module with given `name` into scope of `root`
   *  and give them a source completer for given `src` as type.
   *  In IDE mode: Find all toplevel definitions in `src` and enter then into scope of `root`
   *  with source completer for given `src` as type.
   *  (overridden in interactive.Global).
   */
  def enterToplevelsFromSource(root: Symbol, name: String, src: AbstractFile) {
    enterClassAndModule(root, name, new SourcefileLoader(src))
  }

  /** The package objects of scala and scala.reflect should always
   *  be loaded in binary if classfiles are available, even if sourcefiles
   *  are newer. Late-compiling these objects from source leads to compilation
   *  order issues.
   *  Note: We do a name-base comparison here because the method is called before we even
   *  have ReflectPackage defined.
   */
  def binaryOnly(owner: Symbol, name: String): Boolean =
    name == "package" &&
    (owner.fullName == "scala" || owner.fullName == "scala.reflect")

  /** Initialize toplevel class and module symbols in `owner` from class path representation `classRep`
   */
  def initializeFromClassPath(owner: Symbol, classRep: ClassPath[platform.BinaryRepr]#ClassRep) {
    ((classRep.binary, classRep.source) : @unchecked) match {
      case (Some(bin), Some(src))
      if platform.needCompile(bin, src) && !binaryOnly(owner, classRep.name) =>
        if (settings.verbose) inform("[symloader] picked up newer source file for " + src.path)
        global.loaders.enterToplevelsFromSource(owner, classRep.name, src)
      case (None, Some(src)) =>
        if (settings.verbose) inform("[symloader] no class, picked up source file for " + src.path)
        global.loaders.enterToplevelsFromSource(owner, classRep.name, src)
      case (Some(bin), _) =>
        global.loaders.enterClassAndModule(owner, classRep.name, platform.newClassLoader(bin))
    }
  }

  /**
   * A lazy type that completes itself by calling parameter doComplete.
   * Any linked modules/classes or module classes are also initialized.
   * Todo: consider factoring out behavior from TopClassCompleter/SymbolLoader into
   * supertrait SymLoader
   */
  abstract class SymbolLoader extends SymLoader {

    /** Load source or class file for `root`, return */
    protected def doComplete(root: Symbol): Unit

    def sourcefile: Option[AbstractFile] = None

    /**
     * Description of the resource (ClassPath, AbstractFile)
     * being processed by this loader
     */
    protected def description: String

    private var ok = false

    private def setSource(sym: Symbol) {
      sourcefile foreach (sf => sym match {
        case cls: ClassSymbol => cls.associatedFile = sf
        case mod: ModuleSymbol => mod.moduleClass.associatedFile = sf
        case _ => ()
      })
    }

    override def complete(root: Symbol) {
      try {
        val start = currentTime
        val currentphase = phase
        doComplete(root)
        phase = currentphase
        informTime("loaded " + description, start)
        ok = true
        setSource(root)
        setSource(root.companionSymbol) // module -> class, class -> module
      }
      catch {
        case ex @ (_: IOException | _: MissingRequirementError) =>
          ok = false
          signalError(root, ex)
      }
      initRoot(root)
      if (!root.isPackageClass) initRoot(root.companionSymbol)
    }

    override def load(root: Symbol) { complete(root) }

    private def markAbsent(sym: Symbol): Unit = {
      val tpe: Type = if (ok) NoType else ErrorType

      if (sym != NoSymbol)
        sym setInfo tpe
    }
    private def initRoot(root: Symbol) {
      if (root.rawInfo == this)
        List(root, root.moduleClass) foreach markAbsent
      else if (root.isClass && !root.isModuleClass)
        root.rawInfo.load(root)
    }
  }

  /**
   * Load contents of a package
   */
  class PackageLoader(classpath: ClassPath[platform.BinaryRepr]) extends SymbolLoader with FlagAgnosticCompleter {
    protected def description = "package loader "+ classpath.name

    protected def doComplete(root: Symbol) {
      assert(root.isPackageClass, root)
      root.setInfo(new PackageClassInfoType(newScope, root))

      if (!root.isRoot) {
        for (classRep <- classpath.classes if platform.doLoad(classRep)) {
          initializeFromClassPath(root, classRep)
        }
      }
      if (!root.isEmptyPackageClass) {
        for (pkg <- classpath.packages) {
          enterPackage(root, pkg.name, new PackageLoader(pkg))
        }

        openPackageModule(root)
      }
    }
  }

  class ClassfileLoader(val classfile: AbstractFile) extends SymbolLoader with FlagAssigningCompleter {
    private object classfileParser extends {
      val global: SymbolLoaders.this.global.type = SymbolLoaders.this.global
    } with ClassfileParser

    protected def description = "class file "+ classfile.toString

    protected def doComplete(root: Symbol) {
      val start = if (Statistics.canEnable) Statistics.startTimer(classReadNanos) else null
      classfileParser.parse(classfile, root)
      if (root.associatedFile eq NoAbstractFile) {
        root match {
          // In fact, the ModuleSymbol forwards its setter to the module class
          case _: ClassSymbol | _: ModuleSymbol =>
            debuglog("ClassfileLoader setting %s.associatedFile = %s".format(root.name, classfile))
            root.associatedFile = classfile
          case _ =>
            debuglog("Not setting associatedFile to %s because %s is a %s".format(classfile, root.name, root.shortSymbolClass))
        }
      }
      if (Statistics.canEnable) Statistics.stopTimer(classReadNanos, start)
    }
    override def sourcefile: Option[AbstractFile] = classfileParser.srcfile
  }

  class SourcefileLoader(val srcfile: AbstractFile) extends SymbolLoader with FlagAssigningCompleter {
    protected def description = "source file "+ srcfile.toString
    override def fromSource = true
    override def sourcefile = Some(srcfile)
    protected def doComplete(root: Symbol): Unit = global.currentRun.compileLate(srcfile)
  }

  object moduleClassLoader extends SymbolLoader with FlagAssigningCompleter {
    protected def description = "module class loader"
    protected def doComplete(root: Symbol) { root.sourceModule.initialize }
  }

  /** used from classfile parser to avoid cyclies */
  var parentsLevel = 0
  var pendingLoadActions: List[() => Unit] = Nil
}

object SymbolLoadersStats {
  import scala.reflect.internal.TypesStats.typerNanos
  val classReadNanos = Statistics.newSubTimer  ("time classfilereading", typerNanos)
}
