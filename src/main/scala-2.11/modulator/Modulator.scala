package modulator

import de.sciss.midi._
import Type._


object Modulator{

  def main(args: Array[String]) {

    val path = "C:\\Users\\Madzia\\Modulator\\modulation.mid"
    //------setting tempo------------
    implicit val rate = TickRate.tempo(bpm = 120, tpq = 1024)

    val source = args(0)
    val destination = args(1)
    val sNr = LetterToSound.findSound(source)
    val dNr = LetterToSound.findSound(destination)
    println("pattern matching: " + sNr)

    val kind = DefineModulationType.defineType(sNr,dNr)
    Type.printType(kind)
    var sq = Vector(NoteOn(0,1,50), NoteOff(0,1,0))

    kind match {
      case ModulationType.First => {
        println("first")
        sq = ChordBuilder1.buildSequenceForFirstType(LetterToSound.isMajor(source), LetterToSound.isMajor(destination),sNr,dNr)
      }
      case ModulationType.Second => {
        println("second")
        sq = ChordBuilder2.buildSequenceForSecondType(LetterToSound.isMajor(source), LetterToSound.isMajor(destination),sNr,dNr)
      }
      case ModulationType.Third => {
        println("third")
        sq = ChordBuilder3.buildSequenceForThirdType(LetterToSound.isMajor(source), LetterToSound.isMajor(destination), sNr, dNr)
      }
    }

    val c1 = sq.zipWithIndex.map{case (m,i) => Event((i*0.5*rate.value).toLong, m) }
    val test = sq.zipWithIndex.map{case (m,i) => Event((i*1.5*rate.value).toLong, m) }
    val c2 = Track(c1)
    val test2 = Track(test)
    val c3 = Sequence(Vector(c2))
    c3.write(path)

  }
}
