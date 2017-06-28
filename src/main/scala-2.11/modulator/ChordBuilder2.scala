package modulator

import modulator.ChordBuilder._
import modulator.Type.ModulationType

/**
  * Created by Madzia on 28.06.2017.
  */
object ChordBuilder2 {

  //-----------------drugi typ
  def thirdChord2type(majorDestination: Boolean, array: Array[Int]) = {
    val indexes = findIndexes(majorDestination,array,Array(0,9,6,3),Array(0,9,6,3))
    var map = Map(indexes(0)->0)
    if(majorDestination)
      map = Map(indexes(0) -> 1, indexes(1) -> 1,indexes(2) -> 0,indexes(3) -> -2)
    else
      map = Map(indexes(0) -> 1, indexes(1) -> 0,indexes(2) -> 0,indexes(3) -> -2)

    moveNotes(array,map)
  }

  def fourthChord2Type(majorDestination: Boolean, array: Array[Int]) = {
    val indexes = findIndexes(majorDestination,array,Array(0,9,12,5),Array(0,8,12,5))
    var map = Map(indexes(0)->0)
    if(majorDestination)
      map = Map(indexes(0) -> 0, indexes(1) -> 1,indexes(2) -> 2,indexes(3) -> -1)
    else
      map = Map(indexes(0) -> 0, indexes(1) -> 2,indexes(2) -> 1,indexes(3) -> -1)

    moveNotes(array,map)
  }

  def fifthChord2Type(majorDestination: Boolean, array: Array[Int]) = {
    val indexes = findIndexes(majorDestination,array,Array(0,10,2,4),Array(0,10,1,4))
    var map = Map(indexes(0)->0)
    if(majorDestination)
      map = Map(indexes(0) -> -7, indexes(1) -> -1,indexes(2) -> -2,indexes(3) -> 1)
    else
      map = Map(indexes(0) -> -7, indexes(1) -> -2,indexes(2) -> -1,indexes(3) -> 1)

    moveNotes(array,map)
  }


  def buildSequenceForSecondType(major: Boolean, majorDestination: Boolean, source: Int, destination: Int) = {
    val boxForTonic = tonic(major,source)
    val boxForFirstChord = modulationChord1(ModulationType.Second, source,destination,boxForTonic)

    //musi znalezc od 6 od destination wsrdo 4 dzwiekow z define modula
    //pozostale polaczyc najkrotsza droga  <- 1 akord po tonice

    //przepisanie akordu do nowej tablicy
    val tmp = new Array [Int] (4)
    val tmp2 = new Array [Int] (4)
    val tmp3 = new Array [Int] (4)
    for(i <- 0 to 3) tmp(i) = boxForFirstChord(i)

    val array3 = thirdChord2type(majorDestination,tmp)
    //4 akord
    for(i <- 0 to 3) tmp2(i) = array3(i)
    val array4 = fourthChord2Type(majorDestination,tmp2)
    for(i <- 0 to 3) tmp3(i) = array4(i)
    val array5 = fifthChord2Type(majorDestination,tmp3)

    buildVector(boxForTonic(0),boxForTonic(1),boxForTonic(2),boxForTonic(3)) ++
      buildVector(boxForFirstChord(0),boxForFirstChord(1),boxForFirstChord(2),boxForFirstChord(3)) ++
      buildVector(array3(0),array3(1),array3(2),array3(3)) ++
      buildVector(array4(0),array4(1),array4(2),array4(3)) ++
      buildVector(array5(0),array5(1),array5(2),array5(3))
  }


}
