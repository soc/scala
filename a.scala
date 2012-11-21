// class Outer { class Inner }
trait Locals {
  var field0 = _ // never set
  var field1 = _ // never got

  var field2 = 1 // never set after initialization
  var field3 = 1 // never got after initialization
  var field4 = 1 // never anything after initialization
  var field5 = 1 // used

  field0
  field1 = 1
  field2
  field3 = 5
  field4 = 5
  field5 = 5

  // var fieldx = 1
  // println(fieldx)

  // def f0 = {
  //   var x = 1 // warn
  //   var y = 2
  //   y = 3
  //   y + y
  // }
  // def f1 = {
  //   val a = new Outer // no warn
  //   val b = new Outer // warn
  //   new a.Inner
  // }

  def f0 = { var

  }
  def f2 = {
    var localx = 100 // warn about it being a var
    localx
  }

  // f0
  // f1
  f2
}
