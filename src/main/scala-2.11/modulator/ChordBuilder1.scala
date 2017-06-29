package modulator

import com.sun.corba.se.impl.protocol.giopmsgheaders.Message
import de.sciss.midi.{NoteOff, NoteOn}
import modulator.ChordBuilder.{buildVector1, _}
import modulator.Type.ModulationType


object ChordBuilder1 {
  //kadencja utrwalająca w tonacji docelowej
  def cadenceChord1(majorDestination: Boolean, array: Array[Int], destination: Int) = {
    //nowa pryma akordu
    array(0) += distance(array(0),destination+5)

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

    //taka oktawa zeby skok od array(index1) był najmniejszy
    array(index1) = array(index1) + distance(array(index1),array(0) + 3)
    if(majorDestination)
      array(fifthIndex) = array(fifthIndex) - 2
    else
      array(fifthIndex) = array(fifthIndex) - 1
    array
  }

  def cadenceChord2(majorDestination: Boolean, array: Array[Int]) = {
    val indexes = findIndexes(majorDestination,array,Array(0,7,3,9),Array(0,7,3,9))
    var map = Map(indexes(0)->0)
    var a = 0
    if(majorDestination) a = 2 else a = 1
    map = Map(indexes(0) -> 2, indexes(1) -> 0,indexes(2) -> -1,indexes(3) -> a)

    moveNotes(array,map)
  }

  def cadenceChord3(majorDestination: Boolean, array: Array[Int]) = {
    val indexes = findIndexes(majorDestination,array,Array(0,0,9,5),Array(0,0,8,5))
    var map = Map(indexes(0)->0)
    var a = 0
    if(majorDestination) a = -2 else a = -1
      map = Map(indexes(0) -> -1, indexes(1) -> 2,indexes(2) -> a,indexes(3) -> 0)

    moveNotes(array,map)
  }

  def cadenceChord4(majorDestination: Boolean, array: Array[Int]) = {
    val indexes = findIndexes(majorDestination,array,Array(0,6,3,8),Array(0,6,3,8))
    val map = Map(indexes(0) -> 1, indexes(1) -> -1,indexes(2) -> -4,indexes(3) -> 0)

    moveNotes(array,map)
  }

  def cadenceChord5(majorDestination: Boolean, array: Array[Int]) = {
    val indexes = findIndexes(majorDestination,array,Array(0,7,10,4),Array(0,7,10,4))
    var map = Map(indexes(0)->0)
    var a = 0
    if(majorDestination) a = -1 else a = -2
    map = Map(indexes(0) -> -7, indexes(1) -> -2,indexes(2) -> a,indexes(3) -> 1)
    moveNotes(array,map)
  }

  //funkcja przyjmuje czy tonacja docelowa jest durowa i 1 akord modulacji
  def modulationChord2(major: Boolean, tab: Array[Int]) = {

    tab(0) = tab(0) - 1

    val result = new Array[Int] (4)
    if(major) {result(0) = tab(0) + 2}
    else {result(0) = tab(0) + 1 }

    val third = tab(0) + 4
    val fifth = tab(0) + 7
    val seventh = tab(0) + 10

    for(i <- 1 to 3){
      if(abs(tab(i)-third) %12 == 0) {
        result(i) = tab(i) + 1
      }
      if(abs(tab(i) - fifth) %12 == 0) {
        result(i) = tab(i) - 2
      }
      if(abs(tab(i) - seventh) %12 == 0) {
        if(major) result(i) = tab(i) - 1
        else result(i) = tab(i) - 2
      }
    }
    result
  }

  def buildSequenceForFirstType(major: Boolean, majorDestination: Boolean, source: Int, destination: Int) = {
    val boxForTonic = tonic(major,source)
    val box1 = modulationChord1(ModulationType.First,source,destination,boxForTonic)
    val box2 = box1.map(e => if (box1.indexOf(e) == 0) e-1 else e )
    var result = buildVector(boxForTonic) ++ buildVector(box1) ++ buildVector(box2)
    val box3 = modulationChord2(majorDestination,box1)
    result ++= buildVector(box3)
    val box4 = cadenceChord1(majorDestination,box3,destination)
    result ++= buildVector(box4)
    val box4a = box4.map(e => if (box4.indexOf(e) == 0) e+1 else e)
    result ++= buildVector(box4a)
    val box5 = cadenceChord2(majorDestination,box4)
    result ++= buildVector(box5)
    val box6 = cadenceChord3(majorDestination,box5)
    result ++= buildVector(box6)
    val box7 = cadenceChord4(majorDestination,box6)
    result ++= buildVector(box7) ++ buildVector( cadenceChord5(majorDestination,box7))

    result
  }

  def buildSequence(major: Boolean, majorDestination: Boolean, source: Int, destination: Int) = {
    val boxForTonic = tonic(major,source)
    val box1 = modulationChord1(ModulationType.First,source,destination,boxForTonic)
    val box2 = box1.map(e => if (box1.indexOf(e) == 0) e-1 else e )

    val i = 0
    //var result = new Array[buildVector1(boxForTonic(i))](4)

    for(i <- 0 to 3) {

      var result = buildVector1(boxForTonic(i)) ++ buildVector1(box1(i)) ++ buildVector1(box2(i))
      val box3 = modulationChord2(majorDestination, box1)
      result ++= buildVector1(box3(i))
      val box4 = cadenceChord1(majorDestination, box3, destination)
      result ++= buildVector1(box4(i))
      val box4a = box4.map(e => if (box4.indexOf(e) == 0) e + 1 else e)
      result ++= buildVector1(box4a(i))
      val box5 = cadenceChord2(majorDestination, box4)
      result ++= buildVector1(box5(i))
      val box6 = cadenceChord3(majorDestination, box5)
      result ++= buildVector1(box6(i))
      val box7 = cadenceChord4(majorDestination, box6)
      result ++= buildVector1(box7(i)) ++ buildVector1(cadenceChord5(majorDestination, box7)(i))
    val wynik result
    }
    //result
  }



}
