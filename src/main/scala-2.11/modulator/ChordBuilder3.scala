package modulator

import modulator.ChordBuilder._
import modulator.Type.ModulationType


object ChordBuilder3 {

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

    val index1 = array.indexOf(array.find((distance(array(0)+3,_)==0)).getOrElse(-1))
    val index2 = array.indexOf(array.find((distance(array(0)+9,_)==0)).getOrElse(-1))

    if(index1 < index2)
      array(index2) -= 12

    for (i <- 1 to 3)
      array(i) += 1
    //array(0) -= 1
    //array.map(e=> if(array.indexOf(e)>0)(e+1) else e)

    array
  }
//nie wiem czy to ok???
  /*def fifthChord3Type(majorDestination: Boolean, array: Array[Int]) = {
    val indexes = findIndexes(majorDestination,array,Array(0,3,-1,-1),Array(0,4,-1,-1))
    var map = Map(indexes(0)->0)
    if(majorDestination)
      map = Map(indexes(0) -> 2, indexes(1) -> -1,indexes(2) -> 0,indexes(3) -> 0)
    else
      map = Map(indexes(0) -> 1, indexes(1) -> -2,indexes(2) -> 0,indexes(3) -> 0)

    moveNotes(array,map)
  }*/

  def fifthChord3Type(majorDestination: Boolean, array: Array[Int]) = {

    var found = false
    for (i <- 1 to 3) {
      if(majorDestination) {
        if (distance(array(0) + 3, array(i)) == 0 && found) {
          //nic
        }
        if (distance(array(0) + 3, array(i)) == 0 && !found) {
          array(i) = array(i) + distance(array(i), array(0))
          found = true
        }
      }
      else {
        if (distance(array(0) + 4, array(i)) == 0 && found) {
            //nic
        }
        if (distance(array(0) + 4, array(i)) == 0 && !found) {
          val test = distance(array(i), array(0))
          array(i) = array(i) + test
          found = true
        }
      }
    }
    if(majorDestination) array(0) -= 4
    else array(0) -= 3

    array
  }

  //tu cos zle
  /*def sixthChord3Type(majorDestination: Boolean, array: Array[Int]) = {
    val index = findIndex(majorDestination,array,11,10)
    var map = Map(index->0)
    if(majorDestination)
      map = Map(0-> 1, index->0,index-> 0,index -> -2)
    else
      map = Map(0-> 1, index -> -1,index-> 0,index-> -1)

    moveNotes(array,map)
  }*/


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

  /*def seventhChord3Type(majorDestination: Boolean, array: Array[Int]) = {
    val indexes = findIndexes(majorDestination,array,Array(0,3,8,0),Array(0,8,2,0))
    var map = Map(indexes(0)->0)
    if(majorDestination)
      map = Map(indexes(0) -> 1, indexes(1) -> -2,indexes(2) -> 2,indexes(3) -> 0)
    else
      map = Map(indexes(0) -> 1, indexes(1) -> 1,indexes(2) -> -1,indexes(3) -> 0)

    moveNotes(array,map)
  }*/

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
  def ninthChord3Type(majorDestination: Boolean, array: Array[Int]) = {
  val indexes = findIndexes(majorDestination,array,Array(0,4,10,2),Array(0,4,10,1))
  var map = Map(indexes(0)->0)
  if(majorDestination)
    map = Map(indexes(0) -> -7, indexes(1) -> 1,indexes(2) -> -1,indexes(3) -> -2)
  else
    map = Map(indexes(0) -> -7, indexes(1) -> 1,indexes(2) -> -2,indexes(3) -> -1)

  moveNotes(array,map)
}

  /*def ninthChord3Type(majorDestination: Boolean, array: Array[Int]) = {

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
  }*/

  /*def buildSequenceForThirdType(major: Boolean, majorDestination: Boolean, source: Int, destination: Int) = {
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

  }*/
  def buildSequenceForThirdType(major: Boolean, majorDestination: Boolean, source: Int, destination: Int) = {
    val boxForTonic = tonic(major,source)
    val box1 = modulationChord1(ModulationType.Third, source,destination,boxForTonic)
    var result = buildVector(boxForTonic) ++ buildVector(box1)

    //musi znalezc od 6 od destination wsrdo 4 dzwiekow z define modula
    //pozostale polaczyc najkrotsza droga  <- 1 akord po tonice

    val box2 = thirdChord3Type(majorDestination,box1)
    result ++= buildVector(box2)
    val box3 = czwartyakord3typ(majorDestination,box2)
    result ++= buildVector(box3)
    val box4 = fifthChord3Type(majorDestination,box3)
    result ++= buildVector(box4)
    val box5 = sixthChord3Type(majorDestination,box4)
    result ++= buildVector(box5)
    val box6 = seventhChord3Type(majorDestination,box5)
    result ++= buildVector(box6)
    val box7 = eightChord3Type(majorDestination,box6)
    result ++= buildVector(box7) ++ buildVector(ninthChord3Type(majorDestination,box7))

    result

  }


}
