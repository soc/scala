package scala
package reflect
package api

// trait Guidance[+U <: Universe] {
//   def inferrable(tpe: U#Type): Boolean
// }
// object NoGuidance extends Guidance[Nothing] {
//   def inferrable(tpe: Nothing#Type): Boolean = true
// }

// trait Inferences {
//   def guidance(u: Universe): Guidance[u.type]
// }

trait Guidance {
  def inferrable(tpe: Universe#Type): Boolean
}
object NoGuidance extends Guidance {
  def inferrable(tpe: Universe#Type) = true
}
object NoAnyGuidance extends Guidance {
  def inferrable(tpe: Universe#Type): Boolean = !(tpe exists (tp => tp.typeSymbol.fullName.toString == "scala.Any"))
}

object Guidance {
  val InferenceGuidancePropertyName = "scala.guidance"

  def inferenceGuidance(): Guidance = {
    sys.props(InferenceGuidancePropertyName) match {
      case null      => NoGuidance
      case className =>
        Class.forName(className + "$").getField("MODULE$").get(null).asInstanceOf[Guidance]
        // Class.forName(className).newInstance().asInstanceOf[Guidance]
    }
  }
}

// -Dscala.guide-inference=MyObject