package modulator

import modulator.ChordBuilder._
import modulator.Type.ModulationType


object ChordBuilder3 {

  def thirdChord(majorDestination: Boolean, array: Array[Int]) = {
    array.map(e => if(array.indexOf(e)>0) e+1 else e)
  }

  def fourthChord(majorDestination: Boolean, array: Array[Int]) = {
    val indexes = findIndexes(majorDestination,array,Array(0,10,4,7),Array(0,10,4,7))
    var (a,b) = (1,-2)
    if(majorDestination) {a = 2; b = -2}
    val map = Map(indexes(0) -> a, indexes(1) -> b,indexes(2) -> 1,indexes(3) -> -2)

    moveNotes(array,map)
  }

  def fifthChord(majorDestination: Boolean, array: Array[Int]) = {
    val index = findIndex(majorDestination, array,3,4)
    array(index) += distance( array(index), array(0))
    if(majorDestination) array(0) -= 4
    else array(0) -=3

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


  def seventhChord(majorDestination: Boolean, array: Array[Int]) = {
    val indexes = findIndexes(majorDestination,array,Array(0,3,8,0),Array(0,8,2,0))
    var map = Map(indexes(0)->1)
    if(majorDestination)
      map = Map(indexes(0) -> 0, indexes(1) -> -2,indexes(2) -> 0,indexes(3) -> 1)
    else
      map = Map(indexes(0) -> 0, indexes(1) -> 0,indexes(2) -> -1,indexes(3) -> 1)

    moveNotes(array,map)
  }

  def eightChord(majorDestination: Boolean, array: Array[Int]) = {
    val indexes = findIndexes(majorDestination,array,Array(0,5,9,0),Array(0,5,0,8))
    var map = Map(indexes(0)->0)
    if(majorDestination)
      map = Map(indexes(0) -> 0, indexes(1) -> -1,indexes(2) -> 1,indexes(3) -> 2)
    else
      map = Map(indexes(0) -> 0, indexes(1) -> -1,indexes(2) -> 1,indexes(3) -> 2)
    moveNotes(array,map)
}

  def ninthChord(majorDestination: Boolean, array: Array[Int]) = {

    val indexes = findIndexes(majorDestination,array,Array(0,4,10,2),Array(0,4,10,1))
    var (a,b) = (-2, -1)
    if(majorDestination) {a = -1; b = -2}
    val map = Map(indexes(0) -> -7, indexes(1) -> 1,indexes(2) -> a,indexes(3) ->b)

    val res = moveNotes(array,map)
    if(res(0)<36) res(0) += 12
    res
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


  def buildSequenceForThirdType(major: Boolean, majorDestination: Boolean, source: Int, destination: Int) = {
    val boxForTonic = tonic(major,source)
    val box1 = modulationChord1(ModulationType.Third, source,destination,boxForTonic)
    var result = buildVector(boxForTonic) ++ buildVector(box1)

    //musi znalezc od 6 od destination wsrdo 4 dzwiekow z define modula
    //pozostale polaczyc najkrotsza droga  <- 1 akord po tonice

    val box2 = thirdChord(majorDestination,box1)
    result ++= buildVector(box2)
    val box3 = fourthChord(majorDestination,box2)
    result ++= buildVector(box3)
    val box4 = fifthChord(majorDestination,box3)
    result ++= buildVector(box4)
    val box5 = sixthChord3Type(majorDestination,box4)
    result ++= buildVector(box5)
    val box6 = seventhChord3Type(majorDestination,box5)
    result ++= buildVector(box6)
    val box7 = eightChord(majorDestination,box6)
    result ++= buildVector(box7) ++ buildVector(ninthChord(majorDestination,box7))

    result
  }

}
