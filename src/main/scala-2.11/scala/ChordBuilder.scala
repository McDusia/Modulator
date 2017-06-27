package scala

import de.sciss.midi.{NoteOff, NoteOn}
import Type.ModulationType

object ChordBuilder {

  def buildVector(a: Int, b: Int, c: Int, d: Int) = {
    Vector(
      NoteOn(0,a,127),NoteOn(1,b,127),
      NoteOn(2,c,127), NoteOn(3,d,127),

      NoteOff(0,a,0)//,NoteOff(0,b,0),
      //NoteOff(0,c,0),NoteOff(0,d,0)
    )
  }

  def tonic(major: Boolean, keynote: Int) = {
    val array = new Array[Int](4)
    array(0) = keynote - 12
    array(1) = if(major) keynote + 4 else keynote + 3
    array(2) = keynote + 7
    array(3) = keynote + 12
    array
  }

  def abs(x: Int) : Int = { if (x >= 0) x else -x }

  //def distanceForInt(x: Int, y: Int) = abs(x - y)

  //-----dostaje 3 odleglosci i zwraca najkrótszą jesli istnieje,
  // lub tablice, jeśli jest kilka tak samo małych
  def minJumps(x: Int, y: Int, z: Int): Array[Int] = {
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

  def distance(x: Int, y: Int) = {
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

  def findMinimalChange(rest: Array[Int], restTonic: Array[Int], minus: Boolean) = {

    //------for each note in rest count distances from notes in restTonic:
    val dist0 = new Array[Int](3)
    val dist1 = new Array[Int](3)
    val dist2 = new Array[Int](3)

    //distX - distances X note from notes in restTonic(rest notes from tonic chord)
    for(i <- 0 to 2) {
      dist0(i) = distance(restTonic(0),rest(i))
      dist1(i) = distance(restTonic(1),rest(i))
      dist2(i) = distance(restTonic(2),rest(i))
    }
    //alternatywnie:
    //dist0.map(e=>distance(restTonic(0),rest()))

    var min = 36
    var index1 = -1; var index2 = -1; var index3 = -1;

    for(i <- 0 to 2; j<- 0 to 2;  if(j!=i); k<- 0 to 2; if(k!=i && k!=j)) {
      val t = abs(dist0(i)) + abs(dist1(j)) + abs(dist2(k))

      if((t < min) && !(minus && dist0(i)<0 && dist1(j)<0 && dist2(k)<0) && !(!minus && dist0(i)>0 && dist1(j)>0 && dist2(k)>0)) {
        min = t
        index1 = i
        index2  = j
        index3 = k
      }
    }
    Array(dist0(index1),dist1(index2),dist2(index3))
  }

  def modulationChord1(t: Type.ModulationType.ModulationType ,source:Int, destination:Int, tonic: Array[Int]) = {

    val seventhPitch = source + 11 //tzw. septyma tonacji źródłowej
    val ninthPith = destination + 20  //tzw. nona tonacji docelowej
    val primePith = destination + 6
    val dominantsPrime = destination + 7

    //zbuduj 4 dźwięki od 7 stopnia tonacji wyjściowej
    println("ninth " + ninthPith)
    val tab = new Array [Int](4)

    //obliczenie dźwięków z drugiego akordu
    for(i <- 0 to 3) {tab(i) = seventhPitch + 3 * i; println("tab(i): " + tab(i)) }

    var index = 0
    //znajdz wśród nich dźwiek z tonacji docelowej

    if(t == ModulationType.First) {
      for (i <- 0 to 3)
        if (abs(tab(i) - ninthPith) % 12 == 0) {
          index = i
        }
    }
    if(t == ModulationType.Second) {
      for (i <- 0 to 3)
        if (abs(tab(i) - primePith) % 12 == 0) {
          index = i
        }
    }
    if(t == ModulationType.Third) {
      for (i <- 0 to 3)
        if (abs(tab(i) - dominantsPrime) % 12 == 0) {
          index = i
        }
    }

    //tablica na gotowy akord
    val result = new Array [Int](4)
    //obliczenie basu, tonika -> znaleziony wcześniej dźwięk
    val temporary = distance(tonic(0), tab(index))
    result(0) = tonic(0) + temporary

    //przepisujemy pozostałe dźwięki z akordu toniki do nowej tablicy
    val rest = new Array [Int](3)
    val restTonic = new Array [Int](3)
    var i = 0
    for(j <- 0 to 3; if(j != index)) {rest(i) = tab(j); restTonic(i) = tonic(i+1); i = i+1}

    //znalezienie takiej zamiany pozostałych dźwięków z toniki aby jak najkrótszą drogą przeszły
    //na dźwięki 2 akordu (dodatkowo nie mogą wszystkie iść w górę lub wszystkie w dół - minus)
    val minus = if(temporary<0) true else false
    val mini = findMinimalChange(rest,restTonic, minus)

    for(i <- 1 to 3) result(i) = tonic(i) + mini(i-1)
    result
  }

  def moveNotes(old: Array[Int], toAdd: Map[Int,Int]) = {
    for((a,b) <- Array(0,1,2,3) zip old) yield(b + toAdd.getOrElse(a,-1))
  }

  def findIndexes(majorDestination: Boolean, array: Array[Int], findIfMajor: Array[Int], findIfMinor: Array[Int]) = {
    var indexes = new Array [Int] (4)
    val withoutBass = new Array [Int] (3)
    for(i <- 0 to 2) withoutBass(i) = array(i+1)

    if(majorDestination) {
      for(i <- 1 to 3)
        indexes(i) = withoutBass.indexOf(withoutBass.find((distance(array(0)+findIfMajor(i),_)==0)).getOrElse(-1))
    }
    else {
      for(i <- 1 to 3)
        indexes(i) = withoutBass.indexOf(withoutBass.find((distance(array(0)+findIfMinor(i),_)==0)).getOrElse(-1))
    }
    indexes(0) = -1
    indexes.map(e=> (e+1))
  }

  def printTab(array: Array[Int], size: Int) = {
    println("------------------")
    for (i <- 0 to (size-1)) println("elem(" + i + ")" + array(i))
    println("------------------")
  }

}
