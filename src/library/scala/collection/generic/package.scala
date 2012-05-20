package scala.collection

import generic.CanBuildFrom

package object generic {
  type CanBuild[-Elem, +To] = CanBuildFrom[Nothing, Elem, To]

  /** The type of conversions from a collection representation type
   *  `Repr` to its corresponding TraversableLike.
   *  @see [[scala.collection.generic.FromRepr]]
   */
  type HasElem[Repr, A] = Repr => TraversableLike[A, Repr]
}