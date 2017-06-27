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

  def distanceForInt(x: Int, y: Int) = abs(x - y)

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
    //println("reszta akordu docelowego:")
    //printTab(rest,3)
    //println("reszta toniki:")
    //printTab(restTonic,3)

    //------for each note in rest count distances from notes in restTonic:
    val dist0 = new Array[Int](3)
    val dist1 = new Array[Int](3)
    val dist2 = new Array[Int](3)

    //distX - distances X note from notes in restTonic(rest notes from tonic chord)
    for(i <- 0 to 2) {

      dist0(i) = distance(restTonic(0),rest(i))
      //println("distancesFor0["+i+"]: "+dist0(i))
      dist1(i) = distance(restTonic(1),rest(i))
      //println("distancesFor1["+i+"]: "+dist1(i))
      dist2(i) = distance(restTonic(2),rest(i))
      //println("distancesFor2["+i+"]: "+dist2(i))
    }

    var min = 36
    var index1 = -1; var index2 = -1; var index3 = -1;

    for(i <- 0 to 2; j<- 0 to 2;  if(j!=i); k<- 0 to 2; if(k!=i && k!=j)) {
      val t = abs(dist0(i)) + abs(dist1(j)) + abs(dist2(k))
      //println("testujemy "+t+ "    "+ dist0(i)+"_"+dist1(j)+"_"+dist2(k))

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
          println("note from source key in destination key: " + tab(i))
          index = i
        }
    }
    if(t == ModulationType.Second) {
      for (i <- 0 to 3)
        if (abs(tab(i) - primePith) % 12 == 0) {
          println("note from source key in destination key: " + tab(i))
          index = i
        }
    }
    if(t == ModulationType.Third) {
      for (i <- 0 to 3)
        if (abs(tab(i) - dominantsPrime) % 12 == 0) {
          println("note from source key in destination key: " + tab(i))
          index = i
        }
    }

    //tablica na gotowy akord
    val result = new Array [Int](4)
    //obliczenie basu, tonika -> znaleziony wcześniej dźwięk
    val temporary = distance(tonic(0), tab(index))
    result(0) = tonic(0) + temporary
    //println("pierwszy dzwiek: "+ result(0))
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

  def cadenceChord1(majorDestination: Boolean, array: Array[Int], destination: Int) = {
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

  def cadenceChord2(majorDestination: Boolean,array: Array[Int]) = {

      for(i<-1 to 3)
      {
        if(distance(array(0)+7, array(i))==0) {
          //nic
        }

        if(distance(array(0)+3, array(i))==0) {
          array(i) -= 1
        }
        if(distance(array(0)+9, array(i))==0) {
          if(majorDestination) array(i) += 2
          else array(i) += 1
        }
      }
    array(0) += 2
      array
  }

  def cadenceChord3(majorDestination: Boolean,array: Array[Int]) = {

    println("in cadenceCHORD &&&&&&&&&&&&&&&&&&&&")
    printTab(array,4)

    for(i<-1 to 3)
    {
      if(distance(array(0), array(i))==0) {
        array(i) += 2
      }
      if(majorDestination) {
        if (distance(array(0) + 9, array(i)) == 0) {
          array(i) -= 2
        }
      }
      else {
        if (distance(array(0) + 8, array(i)) == 0) {
          array(i) -= 1
        }
      }

      if(distance(array(0) + 5, array(i))==0) {
        //nic
      }
    }
    array(0) -= 1
    println("poz mainachu%%%%%%%%%%%%%%%%%%")
    printTab(array,4)
    array
  }

  def cadenceChord4(majorDestination: Boolean,array: Array[Int]) = {

    for(i<-1 to 3)
    {
      if (distance(array(0) + 6, array(i)) == 0) {
        array(i) -= 1
      }

      if (distance(array(0) + 3, array(i)) == 0) {
        array(i) -= 4
      }
      if(distance(array(0) + 8, array(i))==0) {
        //nic
      }
    }
    array(0) += 1
    array
  }

  def cadenceChord5(majorDestination: Boolean,array: Array[Int]) = {

    for (i <- 1 to 3) {
      if (distance(array(0) + 7, array(i)) == 0) {
        array(i) -= 2
      }
      if (distance(array(0) + 10, array(i)) == 0) {
        if(majorDestination) array(i) -= 1
        else array(i) -= 2
      }
      if (distance(array(0) + 4, array(i)) == 0) {
        array(i) +=1
      }
    }
    array(0) -= 7
    array
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


  //do sprawdzenia
  def fourthChord3Type(majorDestination: Boolean, array: Array[Int]) = {
    val indexes = findIndexes(majorDestination,array,Array(0,10,4,7),Array(0,10,4,7))
    var map = Map(indexes(0)->0)
    if(majorDestination)
      map = Map(indexes(0) -> 2, indexes(1) -> -1,indexes(2) -> 1,indexes(3) -> -2)
    else
      map = Map(indexes(0) -> 1, indexes(1) -> -2,indexes(2) -> 1,indexes(3) -> -2)

    moveNotes(array,map)
  }

  def czwartyakord3typ(majorDestination: Boolean,array: Array[Int]) = {

    for (i <- 1 to 3) {

      if (distance(array(0) + 10, array(i)) == 0) {
        if(majorDestination) array(i) -= 1
        else array(i) -= 2
      }

      if (distance(array(0) + 4, array(i)) == 0) {
        array(i) += 1
      }
      if (distance(array(0) + 7, array(i)) == 0) {
        array(i) -= 2
      }
    }
    if(majorDestination) array(0) += 2
    else array(0) += 1
    array
  }



  def thirdChord3Type(majorDestination: Boolean, array: Array[Int]) = {

  var index1 = -1
  var index2 = -1

    //index1 = array.indexOf(array.find((distance(array(0)+3,_)==0)).getOrElse(-1))
    //index2 = array.indexOf(array.find((distance(array(0)+9,_)==0)).getOrElse(-1))
    for (i <- 1 to 3) {

      if(distance(array(0)+3, array(i))==0)
        index1 = i
      if(distance(array(0)+9, array(i))==0)
        index2 = i

    }
  if(index1 < index2)
    array(index2) -= 12

    for (i <- 1 to 3)
      array(i) += 1
    //array(0) -= 1
    //array.map(e=> (e+1))

    array
  }

  /*def fifthChord3Type(majorDestination: Boolean, array: Array[Int]) = {
    val indexes = findIndexes(majorDestination,array,Array(0,10,4,7),Array(0,10,4,7))
    var map = Map(indexes(0)->0)
    if(majorDestination)
      map = Map(indexes(0) -> 2, indexes(1) -> -1,indexes(2) -> 1,indexes(3) -> -2)
    else
      map = Map(indexes(0) -> 1, indexes(1) -> -2,indexes(2) -> 1,indexes(3) -> -2)

    moveNotes(array,map)
  }*/

  def fifthChord3Type(majorDestination: Boolean, array: Array[Int]) = {

    println("in PIATY AKORD&&&&&&&&&&&&&&&&&&&&")
    printTab(array,4)

    var found = false

    for (i <- 1 to 3) {
      if(majorDestination) {
        if (distance(array(0) + 3, array(i)) == 0 && found) {
          println("dr " + i + array(i))

        }
        println("test " + distance(array(0) + 3, array(i)))
        if (distance(array(0) + 3, array(i)) == 0 && !found) {
          println("pier" + i + array(i))
          array(i) = array(i) + distance(array(i), array(0))
          found = true
        }
      }
      else {
        if (distance(array(0) + 4, array(i)) == 0 && found) {
          println("dr " + i + array(i))

        }
        println("test " + distance(array(0) + 4, array(i)))
        if (distance(array(0) + 4, array(i)) == 0 && !found) {
          println("pier" + i + array(i))
          array(i) = array(i) + distance(array(i), array(0))
          found = true
        }
      }

    }
    if(majorDestination) array(0) -= 4
    else array(0) -= 3
    println("poz mainachu PIATY%%%%%%%%%%%%%%%%%%")
    printTab(array,4)
    array
  }




  def sixthChord3Type(majorDestination: Boolean, array: Array[Int]) = {

    for (i <- 1 to 3) {
      if(majorDestination) {
        if (distance(array(0) + 11, array(i)) == 0) {
          array(i) -= 2
        }
      }
      else {
        if (distance(array(0) + 10, array(i)) == 0) {
          array(i) -= 1
        }
      }
    }
    array(0) += 1
    array
  }

  def seventhChord3Type(majorDestination: Boolean, array: Array[Int]) = {

    for (i <- 1 to 3) {
      if(majorDestination) {
        if (distance(array(0) + 3, array(i)) == 0) {
          array(i) -= 2
        }
        if (distance(array(0) + 8, array(i)) == 0) {
          array(i) += 2
        }
      }
      else {
        if (distance(array(0) + 8, array(i)) == 0) {
          array(i) += 1
        }
        if (distance(array(0) + 2, array(i)) == 0) {
          array(i) -= 1
        }
      }
    }
    if(majorDestination) array(0) += 1
    else array(0) += 1
    array
  }
//do sprawdzenia
  /*def eightChord3Type(majorDestination: Boolean, array: Array[Int]) = {
  val indexes = findIndexes(majorDestination,array,Array(0,5,9,0),Array(0,5,0,8))
  var map = Map(indexes(0)->0)
  if(majorDestination)
    map = Map(indexes(0) -> 0, indexes(1) -> -1,indexes(2) -> 1,indexes(3) -> 2)
  else
    map = Map(indexes(0) -> 0, indexes(1) -> -1,indexes(2) -> 1,indexes(3) -> 2)

  moveNotes(array,map)
}*/

  def eightChord3Type(majorDestination: Boolean, array: Array[Int]) = {

    for (i <- 1 to 3) {
      if (distance(array(0) + 5, array(i)) == 0) {
        array(i) -= 1
      }
      if(majorDestination) {

        if (distance(array(0) + 9, array(i)) == 0) {
          array(i) += 1
        }
        if (distance(array(0), array(i)) == 0) {
          array(i) += 2
        }
      }
      else {
        if (distance(array(0), array(i)) == 0) {
          array(i) += 1
        }
        if (distance(array(0) + 8, array(i)) == 0) {
          array(i) += 2
        }
      }
    }
    //bas zostaje
    array
  }
//do sprawdzenia
  /*def ninthChord3Type(majorDestination: Boolean, array: Array[Int]) = {
  val indexes = findIndexes(majorDestination,array,Array(0,4,10,2),Array(0,4,10,1))
  var map = Map(indexes(0)->0)
  if(majorDestination)
    map = Map(indexes(0) -> -7, indexes(1) -> 1,indexes(2) -> -1,indexes(3) -> -2)
  else
    map = Map(indexes(0) -> -7, indexes(1) -> 1,indexes(2) -> -2,indexes(3) -> -1)

  moveNotes(array,map)
}*/

  def ninthChord3Type(majorDestination: Boolean, array: Array[Int]) = {

    for (i <- 1 to 3) {
      if (distance(array(0) + 4, array(i)) == 0) {
        array(i) += 1
      }
      if(majorDestination) {
        if (distance(array(0) + 10, array(i)) == 0) {
          array(i) -= 1
        }
        if (distance(array(0) + 2, array(i)) == 0) {
          array(i) -= 2
        }

      }
      else {
        if (distance(array(0) + 10, array(i)) == 0) {
          array(i) -= 2
        }
        if (distance(array(0) + 1, array(i)) == 0) {
          array(i) -= 1
        }
      }
    }
    array(0) -= 7
    println("poz mainachu%%%%%%%%%%%%%%%%%%")
    printTab(array,4)
    array
  }


  def printTab(array: Array[Int], size: Int) = {
    println("------------------")
    for (i <- 0 to (size-1)) println("elem(" + i + ")" + array(i))
    println("------------------")
  }

  //funkcja przyjmuje czy tonacja docelowa jest durowa i 1 akord modulacji
  def modulationChord2(major: Boolean, tab: Array[Int]) = {

    tab(0) = tab(0) - 1
    //println("w ModChord2, array(0)" + array(0) + " array(1) " + array(1) + "array(2)" + array(2))
    println("WZIETY TRZECI AKORD")
    printTab(tab, 4)

    val result = new Array[Int] (4)
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

  def buildSequenceForFirstType(major: Boolean, majorDestination: Boolean, source: Int, destination: Int) = {
    val boxForTonic = tonic(major,source)
    val boxForFirstChord = modulationChord1(ModulationType.First,source,destination,boxForTonic)

    println("EYGSUDFJGS: ")
    printTab(boxForFirstChord,4)
    //przepisanie akordu do nowej tablicy
    val tmp = new Array [Int] (4)
    val temp = new Array [Int] (4)
    val tmp2 = new Array [Int] (4)
    val tmp3 = new Array [Int] (4)
    val tmp4 = new Array [Int] (4)
    val tmp5 = new Array [Int] (4)
    for(i <- 0 to 3) tmp(i) = boxForFirstChord(i)

    val array3 = modulationChord2(majorDestination,tmp)
    //4 akord
    for(i <- 0 to 3) temp(i) = array3(i)
    val array4 = cadenceChord1(majorDestination,temp,destination)
    for(i <- 0 to 3) tmp2(i) = array4(i)
    val array5 = cadenceChord2(majorDestination,tmp2)

    for(i <- 0 to 3) tmp3(i) = array5(i)
    val array6 = cadenceChord3(majorDestination,tmp3)

    for(i <- 0 to 3) tmp4(i) = array6(i)
    val array7 = cadenceChord4(majorDestination,tmp4)

    for(i <- 0 to 3) tmp5(i) = array7(i)
    val array8 = cadenceChord5(majorDestination,tmp5)


    buildVector(boxForTonic(0),boxForTonic(1),boxForTonic(2),boxForTonic(3)) ++
      buildVector(boxForFirstChord(0),boxForFirstChord(1),boxForFirstChord(2),boxForFirstChord(3)) ++
      buildVector(boxForFirstChord(0)-1,boxForFirstChord(1),boxForFirstChord(2),boxForFirstChord(3)) ++
      buildVector(array3(0),array3(1),array3(2),array3(3)) ++
      buildVector(array4(0),array4(1),array4(2),array4(3)) ++
      buildVector(array4(0)+1,array4(1),array4(2),array4(3)) ++
      buildVector(array5(0),array5(1),array5(2),array5(3)) ++
      buildVector(array6(0),array6(1),array6(2),array6(3)) ++
      buildVector(array7(0),array7(1),array7(2),array7(3)) ++
      buildVector(array8(0),array8(1),array8(2),array8(3))
  }



  def buildSequenceForThirdType(major: Boolean, majorDestination: Boolean, source: Int, destination: Int) = {
    val boxForTonic = tonic(major,source)
    val boxForFirstChord = modulationChord1(ModulationType.Third, source,destination,boxForTonic)

    printTab(boxForFirstChord,4)

    //musi znalezc od 6 od destination wsrdo 4 dzwiekow z define modula
    //pozostale polaczyc najkrotsza droga  <- 1 akord po tonice

    //przepisanie akordu do nowej tablicy
    val tmp = new Array [Int] (4)
    val tmp2 = new Array [Int] (4)
    val tmp3 = new Array [Int] (4)
    val tmp4 = new Array [Int] (4)
    val tmp5 = new Array [Int] (4)
    val tmp6 = new Array [Int] (4)
    val tmp7 = new Array [Int] (4)
    for(i <- 0 to 3) tmp(i) = boxForFirstChord(i)

    val array3 = thirdChord3Type(majorDestination,tmp)
    //4 akord
    for(i <- 0 to 3) tmp2(i) = array3(i)
    val array4 = czwartyakord3typ(majorDestination,tmp2)
    for(i <- 0 to 3) tmp3(i) = array4(i)
    val array5 = fifthChord3Type(majorDestination,tmp3)

    for(i <- 0 to 3) tmp4(i) = array5(i)
    val array6 = sixthChord3Type(majorDestination,tmp4)

    for(i <- 0 to 3) tmp5(i) = array6(i)
    val array7 = seventhChord3Type(majorDestination,tmp5)

    for(i <- 0 to 3) tmp6(i) = array7(i)
    val array8 = eightChord3Type(majorDestination,tmp6)

    for(i <- 0 to 3) tmp7(i) = array8(i)
    val array9 = ninthChord3Type(majorDestination,tmp7)

    buildVector(boxForTonic(0),boxForTonic(1),boxForTonic(2),boxForTonic(3)) ++
    buildVector(boxForFirstChord(0),boxForFirstChord(1),boxForFirstChord(2),boxForFirstChord(3)) ++
    buildVector(array3(0),array3(1),array3(2),array3(3)) ++
    buildVector(array4(0),array4(1),array4(2),array4(3)) ++
    buildVector(array5(0),array5(1),array5(2),array5(3)) ++
    buildVector(array6(0),array6(1),array6(2),array6(3)) ++
    buildVector(array7(0),array7(1),array7(2),array7(3)) ++
    buildVector(array8(0),array8(1),array8(2),array8(3)) ++
    buildVector(array9(0),array9(1),array9(2),array9(3))

  }


}
