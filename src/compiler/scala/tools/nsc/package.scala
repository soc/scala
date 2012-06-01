/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools

package object nsc {
  final val NilNil: List[List[Nothing]] = Nil :: Nil

  type Phase = scala.reflect.internal.Phase
  val NoPhase = scala.reflect.internal.NoPhase

  type FatalError = scala.reflect.internal.FatalError
  val FatalError = scala.reflect.internal.FatalError

  type MissingRequirementError = scala.reflect.internal.MissingRequirementError
  val MissingRequirementError = scala.reflect.internal.MissingRequirementError
}
