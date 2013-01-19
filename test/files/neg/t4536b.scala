import language.dynamics

class DynamicClass extends Dynamic {
  def applyDynamic(m: String)() = ()
  nonExistingMethod()
  this.nonExistingMethod()
}
