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
import scala.reflect.NameTransformer
import scala.reflect.internal.MissingRequirementError
import scala.reflect.internal.util.{ Statistics, shortClassOfInstance }
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

  protected def enterIfNew(owner: Symbol, member: Symbol, completer: SymbolLoader): Symbol = {
    if ((owner eq NoSymbol) || (member eq NoSymbol))
      log(s"Tried to enter $member into scope of $owner")
    else if ((owner.info member member.name).alternatives contains member)
      log(s"enterIfNew($owner, $member, _) finds $member already present, " + owner.info.members.mkString(", "))
    else {
      if (owner.info.decls eq EmptyScope)
        log(s"enterIfNew($owner, $member, _) found EmptyScope on $owner")
      else {
        owner.info.decls enter member
        // if (owner.isModule && (owner.moduleClass ne NoSymbol) && (owner.moduleClass.inf
        //   owner.moduleClass.info.decls enter member
        // if (owner.isModuleClass && (owner.sourceModule ne NoSymbol))
        //   owner.sourceModule.info.decls enter member
      }
    }

    member
  }

  /** Enter class with given `name` into scope of `root`
   *  and give them `completer` as type.
   */
  def enterClass(owner: Symbol, name: String, completer: SymbolLoader): Symbol = {
    nestedNames(name) foreach { case ((hd, tl)) =>
      owner.info member (hd: TermName) orElse owner.info.member(hd: TypeName) match {
        case owner1 if (owner1 eq owner)    => return owner
        case owner1 if (owner1 ne NoSymbol) => return enterClass(owner1, tl, completer)
        case _                              =>
      }
    }
    owner.info member (name: TypeName) filter (_.isClass) match {
      case clazz: ClassSymbol =>
        log(s"Found existing class symbol $clazz in $owner")
        clazz setInfo completer
        enterIfNew(owner, clazz, completer)
        // clazz
      case _ =>
        val clazz = owner.newClass(newTypeName(name))
        log(s"Created new class symbol $clazz in $owner")
        clazz setInfo completer
        enterIfNew(owner, clazz, completer)
    }
  }

  private def nestedNames(name: String): Option[(String, String)] = {
    (NameTransformer decode name split '$').toList filterNot (_ == "") match {
      case x :: xs if xs.nonEmpty && !xs.contains("anonfun") && !xs.contains("anon") =>
        val s = NameTransformer encode x
        Some((s, name stripPrefix s + "$"))
      case _ => None
    }
  }

  /** Enter module with given `name` into scope of `root`
   *  and give them `completer` as type.
   */
  def enterModule(owner: Symbol, name: String, completer: SymbolLoader): Symbol = {
    nestedNames(name) foreach { case ((hd, tl)) =>
      owner.info member (hd: TermName) match {
        case owner1 if (owner1 eq owner)    => return owner
        case owner1 if (owner1 ne NoSymbol) => return enterModule(owner1, tl, completer)
        case _                              =>
      }
    }

    if (name endsWith "$")
      enterModuleClass(owner, name, completer).sourceModule
    else owner.info member (name: TermName) filter (_.isModule) match {
      case module: ModuleSymbol =>
        log(s"Found existing module symbol $module in $owner")
        module setInfo completer
        module.moduleClass setInfo moduleClassLoader
        enterIfNew(owner, module, completer)
        // module
      case _ =>
        val module = owner.newModule(name: TermName)
        log(s"Created new module symbol $module in $owner")
        module setInfo completer
        module.moduleClass setInfo moduleClassLoader
        enterIfNew(owner, module, completer)
    }
  }

  private def enterModuleClass(owner: Symbol, name: String, completer: SymbolLoader): Symbol = {
    nestedNames(name) foreach { case ((hd, tl)) =>
      owner.info member (hd: TermName) match {
        case NoSymbol =>
        case owner1   => return logResult(s"Found intermediate owner($owner, $name) == $owner1")(enterModuleClass(owner1.moduleClass, tl, completer))
      }
    }
    val module = owner.info member (name.init: TermName) orElse enterModule(owner, name.init, completer) match {
      case x: ModuleSymbol => x
      case x               => abort(s"ModuleSymbol required here: " + x)
    }
    logResult(s"enterModuleClass(${owner.fullName}, $name, _)")(
      module.moduleClass
        orElse connectModuleToClass(module, owner.newModuleClassSymbol(name: TypeName)).moduleClass
        setInfo completer
    )
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

  /** Enter class and module with given `name` into scope of `root`
   *  and give them `completer` as type.
   */
  def enterClassAndModule(root: Symbol, name: String, completer: SymbolLoader) {
    nestedNames(name) foreach { case ((hd, tl)) =>
      root.info.member(hd: TermName) orElse root.info.member(hd: TypeName) match {
        case NoSymbol =>
        case root1   => logResult(s"Found intermediate owner($root, $name) == $root1")(enterClassAndModule(root1.moduleClass, tl, completer)) ; return
      }
    }
    if (name endsWith "$")
      enterModuleClass(root, name, completer)
    else {
      val clazz  = enterClass(root, name, completer)
      val module = enterModule(root, name, completer)
      if (!clazz.isAnonymousClass) {
        log(s"enterClassAndModule($root, $name, _)")
        assert(clazz.companionModule == module, (clazz.fullLocationString, module.fullLocationString))
        assert(module.companionClass == clazz, (clazz.fullLocationString, module.fullLocationString))
        if ((name != "package$") && (name startsWith "package$")) {
          log(s"Suspiciously entered $clazz and $module based on root=$root, name=$name")
          log(s"... root contains " + root.info.decls.mkString(", "))
          val pkgObject = root.info member nme.PACKAGE
          val modClass = pkgObject.moduleClass

          enterClassAndModule(modClass, name stripPrefix "package$", completer)
        }
      }
    }
  }

  /** In batch mode: Enter class and module with given `name` into scope of `root`
   *  and give them a source completer for given `src` as type.
   *  In IDE mode: Find all toplevel definitions in `src` and enter then into scope of `root`
   *  with source completer for given `src` as type.
   *  (overridden in interactive.Global).
   */
  def enterToplevelsFromSource(root: Symbol, name: String, src: AbstractFile) {
    log(s"enterToplevelsFromSource($root, $name, ${src.path}")
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
    log(s"initializeFromClassPath($owner, $classRep)")
    ((classRep.binary, classRep.source) : @unchecked) match {
      case (Some(bin), Some(src))
      if platform.needCompile(bin, src) && !binaryOnly(owner, classRep.name) =>
        if (settings.verbose.value) inform("[symloader] picked up newer source file for " + src.path)
        global.loaders.enterToplevelsFromSource(owner, classRep.name, src)
      case (None, Some(src)) =>
        if (settings.verbose.value) inform("[symloader] no class, picked up source file for " + src.path)
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
      def signalError(ex: Exception) {
        ok = false
        if (settings.debug.value) ex.printStackTrace()
        val msg = ex.getMessage()
        // SI-5593 Scaladoc's current strategy is to visit all packages in search of user code that can be documented
        // therefore, it will rummage through the classpath triggering errors whenever it encounters package objects
        // that are not in their correct place (see bug for details)
        if (!settings.isScaladoc)
          globalError(
            if (msg eq null) "i/o error while loading " + root.name
            else "error while loading " + root.name + ", " + msg);
      }
      try {
        val start = currentTime
        val currentphase = phase
        doComplete(root)
        phase = currentphase
        informTime("loaded " + description, start)
        ok = true
        setSource(root)
        setSource(root.companionSymbol) // module -> class, class -> module
      } catch {
        case ex: IOException =>
          signalError(ex)
        case ex: MissingRequirementError =>
          signalError(ex)
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
      if (root.rawInfo == this) {
        log(s"initRoot($root) with moduleClass=${root.moduleClass} marked absent")
        List(root, root.moduleClass) foreach markAbsent
      }
      else if (root.isClass && !root.isModuleClass) {
        log(s"initRoot($root) having raw info loaded")
        root.rawInfo.load(root)
      }
      else {
        log(s"initRoot($root) ignored")
      }
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
    private object classfileParser extends ClassfileParser {
      val global: SymbolLoaders.this.global.type = SymbolLoaders.this.global
    }

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
