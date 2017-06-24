package scala

import Type._
object DefineModulationType {
  /*sealed abstract class ModulationType

  class firstType(source:Integer,destination:Integer) extends ModulationType
  class secondType(source:Integer,destination:Integer) extends ModulationType
  class thirdType(source:Integer,destination:Integer) extends ModulationType
  */
  private var secondType = false
  private var thirdType = false
  private var firstType = false

  def abs(x: Integer): Integer = { if (x >= 0) x else -x }

  private def setTypeVariables(source:Integer,destination:Integer) = {
    //odleglosc tonacji jest wielokrotnoscia 3
    firstType = ((source-destination) % 3 == 0)
    //budujemy akord od 7 stopnia
    val seventhPitch = source + 11
    val tab = new Array[Int](4)
    for(i <- 0 to 3) tab(i) = seventhPitch + 3 * i

    for(i <- 0 to 3) {
      if (abs(tab(i) - destination) % 12 == 0) secondType = true
      if (abs(tab(i) - (destination+7)) % 12 == 0) thirdType = true
    }
  }

  def defineType(source:Integer,destination:Integer) = {
    setTypeVariables(source,destination)
    var modType = ModulationType.First
    if (firstType) {
      modType = ModulationType.First
      println("First modulation type")
    }
    if (secondType) {
      modType = ModulationType.Second
      println("Second modulation type")
    }
    if (thirdType) {
      modType = ModulationType.Third
      println("Third modulation type")
    }
    modType
  }

}
