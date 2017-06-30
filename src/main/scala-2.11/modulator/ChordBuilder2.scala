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

  /*def buildSequence(major: Boolean, majorDestination: Boolean, source: Int, destination: Int) = {
    val boxForTonic = tonic(major,source)
    var box1 = modulationChord1(ModulationType.Second,source,destination,boxForTonic)
    var box2 = thirdChord(majorDestination,box1)

    val res0 = extractTrack(0,boxForTonic,box1,box2,majorDestination,destination)
    box1 = modulationChord1(ModulationType.First,source,destination,boxForTonic)
    box2 = box1.map(e => if (box1.indexOf(e) == 0) e-1 else e )
    val res1 = extractTrack(1,boxForTonic,box1,box2,majorDestination,destination)
    box1 = modulationChord1(ModulationType.First,source,destination,boxForTonic)
    box2 = box1.map(e => if (box1.indexOf(e) == 0) e-1 else e )

    val res2 = extractTrack(2,boxForTonic,box1,box2,majorDestination,destination)
    box1 = modulationChord1(ModulationType.First,source,destination,boxForTonic)
    box2 = box1.map(e => if (box1.indexOf(e) == 0) e-1 else e )

    val res3 = extractTrack(3,boxForTonic,box1,box2,majorDestination,destination)

    Array(res0,res1,res2,res3)
    (res0,res1,res2,res3)
  }


  def extractTrack(trackNr: Int,boxForTonic:Array[Int], box1: Array[Int],box2:Array[Int],majorDestination: Boolean,destination: Int) = {

    var result = buildVector1(boxForTonic(trackNr)) ++ buildVector1(box1(trackNr)) ++ buildVector1(box2(trackNr))
    val box3 = modulationChord2(majorDestination, box1)
    result ++= buildVector1(box3(trackNr))
    val box4 = cadenceChord1(majorDestination, box3, destination)
    result ++= buildVector1(box4(trackNr))
    val box4a = box4.map(e => if (box4.indexOf(e) == 0) e + 1 else e)
    result ++= buildVector1(box4a(trackNr))
    val box5 = cadenceChord2(majorDestination, box4)
    result ++= buildVector1(box5(trackNr))
    val box6 = cadenceChord3(majorDestination, box5)
    result ++= buildVector1(box6(trackNr))
    val box7 = cadenceChord4(majorDestination, box6)
    result ++= buildVector1(box7(trackNr)) ++ buildVector1(cadenceChord5(majorDestination, box7)(trackNr))

    result
  }*/


}
