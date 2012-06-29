package scala
package runtime

trait Expecty {
  val assert  = new org.expecty.Expecty()
  val require = new org.expecty.Expecty()
}
