package scala

object Type {
  object ModulationType extends Enumeration {
    type ModulationType = Value
    val First, Second, Third = Value
  }
  import ModulationType._

  def printType(t: ModulationType) = {
    print("Modulation type: ")
    t match {
      case First => println("first")
      case Second => println("second")
      case Third => println("third")
    }
  }

}
