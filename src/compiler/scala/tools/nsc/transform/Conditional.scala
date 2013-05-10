/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package transform

import util.positionedString
import annotation.condition.{ Assertion, AssertLike }


trait ConditionalCompilations {
  self: UnCurry =>

  import global._                  // the global environment
  import definitions._             // standard classes and methods

  private abstract class ConditionLogic {
    def arguments: List[String]
    def default(tp: Type): Outcome
    def lookup(name: String): Type
    def onError(msg: String): Unit

    def subtypeString(tp1: Type, tp2: Type) = (
      "(" + debugString(tp1) + " <:< " + tp2.typeSymbol.simpleName + ")"
    )

    abstract class Condition {
      def isKeep: Boolean
      def name: String
      def tp: Type

      def reason(annotationType: Type) = subtypeString(annotationType, tp)
      def keeps(annotationType: Type) = false
      def drops(annotationType: Type) = false
    }
    case class Keep(name: String) extends Condition {
      def isKeep = true
      lazy val tp = lookup(name)
      override def keeps(annotationType: Type) = annotationType <:< tp
      override def toString = "+" + name + " (tp = " + tp + ")"
    }
    case class Drop(name: String) extends Condition {
      def isKeep = false
      lazy val tp = lookup(name)
      override def drops(annotationType: Type) = annotationType <:< tp
      override def toString = "-" + name + " (tp = " + tp + ")"
    }
    case class Error(name: String) extends Condition {
      def isKeep = false
      def tp = NoType
    }
    case class Outcome(keep: Boolean, why: String) { }

    lazy val conditionList: List[Condition] = {
      val conds = arguments map {
        case x if x.head == '+' => Keep(x.tail)
        case x if x.head == '-' => Drop(x.tail)
        case x                  => onError("argument not understood: " + x) ; Error(x)
      }
      log("conditionList: " + conds)
      conds
    }
    def evaluate(annotationType: Type): Outcome = {
      conditionList.foldLeft(default(annotationType)) { (res, cond) =>
        if (res.keep && cond.drops(annotationType)) Outcome(false, cond.reason(annotationType))
        else if (!res.keep && cond.keeps(annotationType)) Outcome(true, cond.reason(annotationType))
        else res
      }
    }
  }

  trait ConditionalTransformer {
    self: UnCurryTransformer =>

    private lazy val logic: ConditionLogic = new ConditionLogic {
      val arguments = (settings.condList.value split ',').toList map (_.trim) filterNot (_== "")
      def default(tp: Type): Outcome = Outcome(true, "default policy")
      def lookup(name: String): Type = wellKnownName(name) orElse safeGetType(name) getOrElse NoType
      def onError(msg: String): Unit = log("Error: " + msg)

      private def safeGetMember(owner: Symbol, path: String): Option[Symbol] = {
        try Some(definitions.getClass(owner.fullName + "." + path))
        catch { case _: MissingRequirementError => None }
      }
      private def safeGetType(name: String): Option[Type] = {
        try Some(definitions.getClass(name).tpe)
        catch { case _: MissingRequirementError => None }
      }
      private def wellKnownName(name: String): Option[Type] = {
        val x = name.toTypeName
        List(ConditionPackage, ScalaPackage) foreach { owner =>
          val res = safeGetMember(owner, x.toString)
          if (res.isDefined)
            return res map (_.tpe)
        }
        None
      }
    }
    import logic.{ Outcome, conditionList }

    private def noAssertionsSet       = !settings.noassertions.isDefault
    private def elideBelowSet         = !settings.elidebelow.isDefault
    //
    // private lazy val disableAssertionType =
    //   wellKnownName("AssertLike") getOrElse Predef.error("AssertLike not found.")

    // private lazy val conditionalIncludes: List[Type] = safeGetTypes(settings.condInclude.value)
    // private lazy val conditionalExcludes: List[Type] = {
    //   val given = safeGetTypes(settings.condExclude.value)
    //
    //   if (noAssertionsSet) given :+ disableAssertionType
    //   else given
    // }

    /** TODO: determine logic for these.
     */
    // private def defaultIsKeep(tp: Type): Boolean = {
    //   true
    // }
    // private def defaultCondition(tp: Type): Condition = Default(true, tp)
    private def replacement(resultType: Type): Tree = {
      gen.mkZero(resultType)
    }

    /** If neither -Xinclude or -Xexclude is given, do nothing for now.
     */
    // private lazy val conditionalEnabled = conditionalList.nonEmpty
    // {
    //   (conditionalList.nonEmpty || elideBelowSet) && {
    //
    //     log("@conditional includes: " + conditionalIncludes.mkString(" "))
    //     log("@conditional excludes: " + conditionalExcludes.mkString(" "))
    //     true
    //   }
    // }

    private def isElidableAnnotated(tree: Tree): Boolean = {
      val sym = tree.symbol

      (!settings.elidebelow.isDefault) &&
      (sym hasAnnotation ElidableMethodClass) &&
      (sym.elisionLevel.exists(_ < settings.elidebelow.value)) && {
        log(positionedString(tree.pos, "eliding call to %s due to -Xelide-below option.".format(sym)))
        true
      }
    }

    /** Returns true if the method's @conditional annotation considered next to
     *  any given command line options leads to the doom of this method invocation.
     */
    def isElidable(tree: Tree): Boolean = {
      // fast failure
      if (conditionList.isEmpty || !tree.symbol.isConditional)
        return isElidableAnnotated(tree)

      val sym = tree.symbol
      val condType = tree match {
        case Apply(sel @ Select(qual, _), _)  =>
          qual.tpe.memberInfo(sym.conditionalType.typeSymbol)
        case _ =>
          sym.conditionalType
      }
      log("@conditional: " + sym + " appears to have @conditional type " + debugString(condType))
      val Outcome(isKeep, why) = logic.evaluate(condType)

      log(positionedString(tree.pos,
        if (isKeep) "retaining conditional call to %s due to %s".format(sym, why)
        else "eliding call to %s due to %s".format(sym, why)
      ))

      !isKeep
      // def exclusionReason(arg: Type) = (
      //   if (noAssertionsSet && arg <:< disableAssertionType) "-Xdisable-assertions option"
      //   else "-Xexclude argument " + subtypeString(condType, arg)
      // )
      // if (defaultIsKeep(condType)) {
      //   conditionalExcludes.find(condType <:< _) match {
      //     case Some(arg)  => finish(true, exclusionReason(arg))
      //     case _          => finish(false, "default policy")
      //   }
      // }
      // else {
      //   conditionalIncludes.find(condType <:< _) match {
      //     case Some(arg)  => finish(false, "-Xinclude argument " + subtypeString(condType, arg))
      //     case _          => finish(true, "default policy")
      //   }
      // }
    }
    /** Substitutes a replacement tree for the given apply call.
     */
    def performElision(tree: Tree): Tree = tree match {
      case Apply(fn, _) => replacement(fn.tpe.resultType)
      case Select(_, _) => replacement(tree.tpe.resultType)
      case _            =>
        log("Tried to elide inappropriate node: " + tree)
        tree
    }
  }
}
