package modulator

import modulator.ChordBuilder._
import modulator.Type.ModulationType

/**
  * Created by Madzia on 28.06.2017.
  */
object ChordBuilder1 {
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

  //funkcja przyjmuje czy tonacja docelowa jest durowa i 1 akord modulacji
  def modulationChord2(major: Boolean, tab: Array[Int]) = {

    tab(0) = tab(0) - 1

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

    result
  }

  def buildSequenceForFirstType(major: Boolean, majorDestination: Boolean, source: Int, destination: Int) = {
    val boxForTonic = tonic(major,source)
    val boxForFirstChord = modulationChord1(ModulationType.First,source,destination,boxForTonic)

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

}
