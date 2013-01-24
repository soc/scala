trait Parent {
  override def toString = "blubber"
}

trait Simple extends Parent {
  def test = toString
}
