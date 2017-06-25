package scala

import de.sciss.midi.{NoteOff, NoteOn}


object ChordBuilder {

  def buildVector(a: Integer, b: Integer, c: Integer, d: Integer) = {
    Vector(
      NoteOn(0,a,50),NoteOn(0,b,50),
      NoteOn(0,c,50), NoteOn(0,d,50),

      NoteOff(0,a,0)//,NoteOff(0,b,0),
      //NoteOff(0,c,0),NoteOff(0,d,0)
    )
  }

  def tonic(major: Boolean, keynote: Integer) = {
    val array = new Array[Integer](4)
    array(0) = keynote - 12
    array(1) = if(major) keynote + 4 else keynote + 3
    array(2) = keynote + 7
    array(3) = keynote + 12
    array
  }

  def abs(x: Integer) : Integer = { if (x >= 0) x else -x }

  def distanceForInt(x: Integer, y: Integer) = abs(x - y)

  //-----dostaje 3 odleglosci i zwraca najkrótszą jesli istnieje,
  // lub tablice, jeśli jest kilka tak samo małych
  def minJumps(x: Integer, y: Integer, z: Integer): Array[Integer] = {
    val a = abs(x)
    val b = abs(y)
    val c = abs(z)

    if (a < b && a < c) Array(x)
    else if (b < a && b < c) Array(y)
    else if ((a == b) && (b < c)) Array(x,y)
    else if ((b == c) && (c < a)) Array(y,z)
    else if ((a == c) && (c < b)) Array(x,z)
    else Array(z)

  }

  def distance(x: Integer, y: Integer) = {
    val a = (y - x)%12
    if(a > 0) {
      //skok w górę
      if (a <= 6) a
      else a - 12
    }
    else {
      //skok w dół
      if (a >= -6) a
      else a + 12
    }
  }

  def findMinimalChange(rest: Array[Integer], restTonic: Array[Integer], size: Integer, minus: Boolean) = {
    println("reszta akordu docelowego:")
    printTab(rest,3)
    println("reszta toniki:")
    printTab(restTonic,3)

    if(size != 3) println("error in findMinimalChange")  //wyczyścić
    //------for each note in rest count distances from notes in restTonic:
    val dist0 = new Array[Integer](3)
    val dist1 = new Array[Integer](3)
    val dist2 = new Array[Integer](3)

    //distX - distances X note from notes in restTonic(rest notes from tonic chord)
    for(i <- 0 to 2) {

      dist0(i) = distance(restTonic(0),rest(i))
      println("distancesFor0["+i+"]: "+dist0(i))
      dist1(i) = distance(restTonic(1),rest(i))
      println("distancesFor1["+i+"]: "+dist1(i))
      dist2(i) = distance(restTonic(2),rest(i))
      println("distancesFor2["+i+"]: "+dist2(i))
    }

   var min = 36
    var index1 = -1; var index2 = -1; var index3 = -1;
  println("ti")
    //----check witch sum will be the smallest
    //if(j!=i) , ; if(k!=i && k!=j)
    for(i <- 0 to 2; j<- 0 to 2;  if(j!=i); k<- 0 to 2; if(k!=i && k!=j)) {
      val t = abs(dist0(i)) + abs(dist1(j)) + abs(dist2(k))
      println("testujemy "+t+ "    "+ dist0(i)+"_"+dist1(j)+"_"+dist2(k))

      if((t < min) && !(minus && dist0(i)<0 && dist1(j)<0 && dist2(k)<0) && !(!minus && dist0(i)>0 && dist1(j)>0 && dist2(k)>0)) {
        min = t
        index1 = i
        index2  = j
        index3 = k
      }
    }
    println("znalezione min"+ min)
    println("ti")
    val a = Array(dist0(index1),dist1(index2),dist2(index3))
    println("in findMinimalChange")
    printTab(a,3)
    a
  }

  def modulationChord1(source:Integer, destination:Integer, tonic: Array[Integer]) = {

    println("***************** In modChord1 *******************")
    printTab(tonic,4)
    val seventhPitch = source + 11 //tzw. septyma tonacji źródłowej
    val ninthPith = destination + 20  //tzw. nona tonacji docelowej

    //zbuduj 4 dźwięki od 7 stopnia tonacji wyjściowej
    println("ninth " + ninthPith)
    val tab = new Array [Integer](4)

    //obliczenie dźwięków z drugiego akordu
    for(i <- 0 to 3) {tab(i) = seventhPitch + 3 * i; println("tab(i): " + tab(i)) }

    var index = 0
    //znajdz wśród nich dźwiek z tonacji docelowej
    for(i <- 0 to 3)
      if(abs(tab(i)-ninthPith) % 12 == 0)
      {
        println("note from source key in destination key: " + tab(i))
        index = i
      }
    //--------------------------
    //tablica na gotowy akord
    val result = new Array [Integer](4)
    //obliczenie basu, tonika -> znaleziony wcześniej dźwięk
    val temporary = distance(tonic(0), tab(index))
    result(0) = tonic(0) + temporary
    println("pierwszy dzwiek: "+ result(0))
    //przepisujemy pozostałe dźwięki z akordu toniki do nowej tablicy
    val rest = new Array [Integer](3)
    val restTonic = new Array [Integer](3)
    var i = 0
    for(j <- 0 to 3; if(j != index)) {rest(i) = tab(j); restTonic(i) = tonic(i+1); i = i+1}

    //znalezienie takiej zamiany pozostałych dźwięków z toniki aby jak najkrótszą drogą przeszły
    //na dźwięki 2 akordu (dodatkowo nie mogą wszystkie iść w górę lub wszystkie w dół - minus)
    val minus = if(temporary<0) true else false
    println(minus)
    val mini = findMinimalChange(rest,restTonic,rest.length, minus)

    println("STOP")
    printTab(mini,3)
    //-----------------------------------------

    println("tonika:")
    printTab(tonic,4)
    result(1) = tonic(1) + mini(0)
    result(2) = tonic(2) + mini(1)
    result(3) = tonic(3) + mini(2)

    println("DRUGI AKORD")
    printTab(result,4)

    result
  }

  def cadenceChord1(majorDestination: Boolean, array: Array[Integer], destination: Integer) = {
    //nowa pryma akordu

    array(0) = array(0) + distance(array(0),destination+5)
    println("nowa pryma: "+ array(0))
    //znalezienie 2 tercji akordu, 2 dźwięki takie same
    var third1 = array(0)
    var third2 = array(0)
    var index1 = -1
    var index2 = -1
    var fifthIndex = array(0)
    if(distance(array(1),array(2)) == 0) {
      third1 = array(1)
      third2 = array(2)
      index1 = 1
      index2 = 2
      fifthIndex = 3
    }
    else if(distance(array(1),array(3)) == 0){
      third1 = array(1)
      index1 = 1
      third2 = array(3)
      index2 = 3
      fifthIndex = 2
    }
    else {
      third1 = array(2)
      index1 = 2
      third2 = array(3)
      index2 = 3
      fifthIndex = 1
    }
    println("znaleziona tercja: " + third1)
    println("druga tercja: " + third2)
    println("kwinta: " + array(fifthIndex))
    //taka oktawa zeby skok od array(index1) był najmniejszy
    array(index1) = array(index1) + distance(array(index1),array(0) + 3)
    if(majorDestination)
      array(fifthIndex) = array(fifthIndex) - 2
    else
      array(fifthIndex) = array(fifthIndex) - 1
    array
  }

  def cadenceChord2(array: Array[Integer]) = {

  }
  def printTab(array: Array[Integer], size: Integer) = {
    println("------------------")
    for (i <- 0 to (size-1)) println("elem(" + i + ")" + array(i))
    println("------------------")
  }

  //funkcja przyjmuje czy tonacja docelowa jest durowa i 1 akord modulacji
  def modulationChord2(major: Boolean, tab: Array[Integer]) = {

    tab(0) = tab(0) - 1
    //println("w ModChord2, array(0)" + array(0) + " array(1) " + array(1) + "array(2)" + array(2))
    println("WZIETY TRZECI AKORD")
    printTab(tab, 4)

    val result = new Array[Integer] (4)
    if(major) {result(0) = tab(0) + 2}
      else {result(0) = tab(0) + 1 }

    val third = tab(0) + 4
    printf("tercja: "+ third)
    val fifth = tab(0) + 7
    println("kwinta: "+ fifth)
    val seventh = tab(0) + 10
    println("septyma" + seventh)

    for(i <- 1 to 3){
      if(abs(tab(i)-third) %12 == 0) {
        result(i) = tab(i) + 1
        val index = i
      }
      if(abs(tab(i) - fifth) %12 == 0) {
        result(i) = tab(i) - 2
      }
      if(abs(tab(i) - seventh) %12 == 0) {
        if(major) result(i) = tab(i) - 1
        else result(i) = tab(i) - 2
      }
    }
    println("CZWARTY AKORD")
    printTab(result,4)

    result
  }

  def buildSequence(major: Boolean, majorDestination: Boolean, source: Integer, destination: Integer) = {
    val boxForTonic = tonic(major,source)
    val boxForFirstChord = modulationChord1(source,destination,boxForTonic)

    println("EYGSUDFJGS: ")
    printTab(boxForFirstChord,4)
    //przepisanie akordu do nowej tablicy
    val tmp = new Array [Integer] (4)
    for(i <- 0 to 3) tmp(i) = boxForFirstChord(i)

    val array3 = modulationChord2(majorDestination,tmp)
    //4 akord
    for(i <- 0 to 3) tmp(i) = array3(i)
    val array4 = cadenceChord1(majorDestination,tmp,destination)

    println("TEST")
    println("TONIKA")
    printTab(boxForTonic,4)
    println("DRUGI AKORD")
    printTab(boxForFirstChord,4)
    //println("CZWARTY AKORD")
    //printTab(array3,4)

    buildVector(boxForTonic(0),boxForTonic(1),boxForTonic(2),boxForTonic(3)) ++
      buildVector(boxForFirstChord(0),boxForFirstChord(1),boxForFirstChord(2),boxForFirstChord(3)) ++
      buildVector(boxForFirstChord(0)-1,boxForFirstChord(1),boxForFirstChord(2),boxForFirstChord(3)) ++
      buildVector(array3(0),array3(1),array3(2),array3(3)) ++
      buildVector(array4(0),array4(1),array4(2),array4(3))

  }

}
