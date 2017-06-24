package scala

object LetterToSound {

  //----returns true if it is Major, otherwise (is Minor) false
  def isMajor(letter:String) : Boolean = {
    val a = letter.charAt(0)
    //val b = a.isLetter && a.isUpper
    a.isUpper
  }

  def findSound(letter:String) : Int = letter.toLowerCase() match {
    case "c" => 60
    case "cis" => 61
    case "des" => findSound("cis")
    case "d" => 62
    case "dis" => 63
    case "es" => findSound("dis")
    case "e" => 64
    case "eis" => findSound("f")
    case "fes" => findSound("e")
    case "f" => 53
    case "fis" => 54
    case "ges" => findSound("fis")
    case "g" => 55
    case "gis" => 56
    case "as" => findSound("gis")
    case "a" => 57
    case "ais" => 58
    case "b" => findSound("ais")
    case "h" => 59
    case "his" => findSound("c")
    case "ces" => findSound("h")
    case _ => println("Error in findSound"); 0
  }

}
