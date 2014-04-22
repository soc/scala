package scala
package collection
import generic.CanBuildFrom

import scala.language.higherKinds

package object generic {
  type CanBuild[-Elem, +To] = CanBuildFrom[Nothing, Elem, To]
}
