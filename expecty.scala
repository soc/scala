package scala

trait Expecty {
  object assert extends org.expecty.Expecty()
  object require extends org.expecty.Expecty()
}
