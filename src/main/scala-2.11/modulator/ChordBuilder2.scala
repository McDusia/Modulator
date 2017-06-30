package modulator

import modulator.ChordBuilder._
import modulator.Type.ModulationType


object ChordBuilder2 {

  def thirdChord(majorDestination: Boolean, array: Array[Int]) = {
    val indexes = findIndexes(majorDestination,array,Array(0,9,6,3),Array(0,9,6,3))
    var a = 0
    if(majorDestination) a = 1
    val map = Map(indexes(0) -> 1, indexes(1) -> a,indexes(2) -> 0,indexes(3) -> -2)
    moveNotes(array,map)
  }

  def fourthChord(majorDestination: Boolean, array: Array[Int]) = {
    val indexes = findIndexes(majorDestination,array,Array(0,9,12,5),Array(0,8,12,5))
    var (a,b) = (2,1)
    if(majorDestination)  {a = 1; b =2}
    val map = Map(indexes(0) -> 0, indexes(1) -> a,indexes(2) -> b,indexes(3) -> -1)
    moveNotes(array,map)
  }

  def fifthChord(majorDestination: Boolean, array: Array[Int]) = {
    val indexes = findIndexes(majorDestination,array,Array(0,10,2,4),Array(0,10,1,4))
    var (a,b) = (-2,-1)
    if(majorDestination) {a = -1; b = -2}
    val map = Map(indexes(0) -> -7, indexes(1) -> a,indexes(2) -> b,indexes(3) -> 1)
    moveNotes(array,map)
  }

  def buildSequenceForSecondType(major: Boolean, majorDestination: Boolean, source: Int, destination: Int) = {

    val boxForTonic = tonic(major,source)
    val box1 = modulationChord1(ModulationType.Second, source,destination,boxForTonic)

    //musi znalezc od 6 od destination wśród 4 dźwieków z define modula
    //pozostale polaczyc najkrotsza droga  <- 1 akord po tonice
    var result = buildVector(boxForTonic) ++ buildVector(box1)
    val box2 = thirdChord(majorDestination,box1)
    result ++= buildVector(box2)
    val box3 = fourthChord(majorDestination,box2)
    result ++= buildVector(box3) ++ buildVector(fifthChord(majorDestination,box3))

    result
  }

}
