package scala

import de.sciss.midi._
import Type._

object scala{

  def main(args: Array[String]) {

    val path = "C:\\Users\\Madzia\\Modulator\\test.mid"
    //------setting tempo------------
    implicit val rate = TickRate.tempo(bpm = 120, tpq = 512)


    val source = args(0)
    val destination = args(1)
    val sNr = LetterToSound.findSound(source)
    val dNr = LetterToSound.findSound(destination)
    println("pattern matching: " + sNr)


    val kind = DefineModulationType.defineType(sNr,dNr)
    Type.printType(kind)
    var sq = Vector(
      NoteOn(0,1,50),
      NoteOff(0,1,0)
    )
    kind match {
      case ModulationType.First => {
        println("first")
        sq = ChordBuilder.buildSequenceForFirstType(LetterToSound.isMajor(source), LetterToSound.isMajor(destination),sNr,dNr)
      }
      case ModulationType.Second => {
        println("second")
        sq = ChordBuilder.buildSequenceForSecondType(LetterToSound.isMajor(source), LetterToSound.isMajor(destination),sNr,dNr)
      }
      case ModulationType.Third => {
        println("third")
        sq = ChordBuilder.buildSequenceForThirdType(LetterToSound.isMajor(source), LetterToSound.isMajor(destination), sNr, dNr)
      }
    }


    val c1 = sq.zipWithIndex.map{case (m,i) => Event((i*rate.value).toLong, m) }
    val c2 = Track(c1)
    val c3 = Sequence(Vector(c2))
    c3.write(path)


    //ChordBuilder.changeNote(Array(1,8,30,4),Array(1,2,1,5))
    //ChordBuilder.tescik(true,Array(1,2,3,4))

  }
}
