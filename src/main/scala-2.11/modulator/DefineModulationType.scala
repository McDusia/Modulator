package modulator

import Type._
object DefineModulationType {

  private var secondType = false
  private var thirdType = false
  private var firstType = false

  def abs(x: Int): Int = { if (x >= 0) x else -x }

  private def setTypeVariables(source:Int,destination:Int) = {
    //odleglosc tonacji jest wielokrotnoscia 3 półtonów
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

  def defineType(source:Int,destination:Int) = {
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
