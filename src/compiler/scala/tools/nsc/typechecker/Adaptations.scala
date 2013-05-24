/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package typechecker

/** This trait provides logic for assessing the validity of argument
 *  adaptations, such as tupling, unit-insertion, widening, etc.  Such
 *  logic is spread around the compiler, without much ability on the
 *  part of the user to tighten the potentially dangerous bits.
 *
 *  TODO: unifying/consolidating said logic under consistent management.
 *
 *  @author  Paul Phillips
 */
trait Adaptations {
  self: Analyzer =>

  import global._
  import definitions._

  trait Adaptation {
    self: Typer =>

    def checkValidAdaptation(t: Tree, args: List[Tree]): Boolean = {
      val m = t.symbol
      def applyArg = t match {
        case Apply(_, arg :: Nil) => arg
        case _                    => EmptyTree
      }
      def prefix = t match {
        case Apply(qual, _) => qual.tpe
        case _              => m.owner.tpe
      }
      def callString = (
        ( if (m.isConstructor) "new " else "" ) +
        ( m.owner.decodedName ) +
        ( if (m.isConstructor || m.name == nme.apply) "" else "." + m.decodedName )
      )
      def sigString = (
          m.owner.decodedName
        + ( if (m.isConstructor) "" else "." + m.decodedName )
        + m.signatureString
      )
      def givenString = if (args.isEmpty) "<none>" else args.mkString(", ")
      def adaptedArgs = if (args.isEmpty) "(): Unit" else args.mkString("(", ", ", "): " + applyArg.tpe)

      def adaptWarning(msg: String) = context.warning(t.pos, msg +
        "\n        signature: " + sigString +
        "\n  given arguments: " + givenString +
        "\n after adaptation: " + callString + "(" + adaptedArgs + ")"
      )
      // A one-argument method accepting Object (which may look like "Any"
      // at this point if the class is java defined) is a "leaky target" for
      // which we should be especially reluctant to insert () or auto-tuple.
      def isLeakyTarget = {
        val oneArgObject = m.paramss match {
          case (param :: Nil) :: Nil  => ObjectClass isSubClass param.tpe.typeSymbol
          case _                      => false
        }
        // Unfortunately various "universal" methods and the manner in which
        // they are used limits our ability to enforce anything sensible until
        // an opt-in compiler option is given.
        oneArgObject && !(
             isStringAddition(m)
          || isArrowAssoc(m)
          || m.name == nme.equals_
          || m.name == nme.EQ
          || m.name == nme.NE
        )
      }
      val mm = m.owner.info member m.name
      val isLeakyOverload = isLeakyTarget && mm.isOverloaded
      println("m = " + m.defString + " " + mm.defString)

      withAddendum(applyArg.pos)(
        "Refusing to tuple argument list for leaky (Object-receiving) overloaded method. Wrap it in ((double parentheses)) if it is what you intended."
      )
      if (isLeakyOverload) {
        def msg = "Refusing to tuple argument list for leaky (Object-receiving) overloaded method. Wrap it in ((double parentheses)) if it is what you intended."
        adaptWarning(msg)
        context.error(t.pos, msg)
        // TyperErrorGen.AdaptTypeError(t, t.tpe, mm.tpe)
        // setError(t)
      }
      else if (settings.noAdaptedArgs)
        adaptWarning("No automatic adaptation here: use explicit parentheses.")
      else if (settings.warnAdaptedArgs)
        adaptWarning(
          if (args.isEmpty) "Adapting argument list by inserting (): " + (
            if (isLeakyTarget) "leaky (Object-receiving) target makes this especially dangerous."
            else "this is unlikely to be what you want."
          )
          else s"Adapting argument list by creating a ${args.size}-tuple: this may not be what you want."
        )

      !settings.noAdaptedArgs && !isLeakyOverload
    }
  }
}
