package scala

import de.sciss.midi._
import Type._

object scala{

  def main(args: Array[String]) {

    val path = "C:\\Users\\Madzia\\Modulator\\test.mid"
    //------setting tempo------------
    implicit val rate = TickRate.tempo(bpm = 120, tpq = 512)

    //----TEST pattern matching----

    val source = args(0)
    val destination = args(1)
    val sNr = LetterToSound.findSound(source)
    val dNr = LetterToSound.findSound(destination)
    println("pattern matching: " + sNr)

    //----TEST ChordBuilder--------
    //zalozenie ze wszystko jest 1 typem

    val debug = ChordBuilder.minJumps(1,2,1)
    println("**************TEST******   " )
    val t = ChordBuilder.printTab(debug,debug.size)
    //val t2 = ChordBuilder.distance(44,)
    //val t1 = ChordBuilder.findMinimalChange(Array(36,41,47),Array(42,38,46),3)

    //val sq = ChordBuilder.buildSequenceForFirstType(LetterToSound.isMajor(source), LetterToSound.isMajor(destination),sNr,dNr)
    val sq = ChordBuilder.buildSequenceForSecondType(LetterToSound.isMajor(source), LetterToSound.isMajor(destination),sNr,dNr)
    println("mod 1 type test "+ sq)
    val c1 = sq.zipWithIndex.map{case (m,i) => Event((i*rate.value).toLong, m) }
    val c2 = Track(c1)
    val c3 = Sequence(Vector(c2))
    c3.write(path)

    val kind = DefineModulationType.defineType(sNr,dNr)
    Type.printType(kind)

  }
}
