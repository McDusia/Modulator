package modulator

import de.sciss.midi._
import Type._


object Modulator{

  def main(args: Array[String]) {

    val path = "C:\\Users\\Madzia\\Modulator\\modulation.mid"
    val path2 = "C:\\Users\\Madzia\\Modulator\\modulation2.mid"
    //------setting tempo------------
    implicit val rate = TickRate.tempo(bpm = 120, tpq = 1024)

    val source = args(0)
    val destination = args(1)
    val sNr = LetterToSound.findSound(source)
    val dNr = LetterToSound.findSound(destination)
    println("source note number: " + sNr)

    val kind = DefineModulationType.defineType(sNr,dNr)

    var sq = Vector(NoteOn(0,1,50), NoteOff(0,1,0))
    kind match {
      case ModulationType.First => {
        sq = ChordBuilder1.buildSequenceForFirstType(LetterToSound.isMajor(source), LetterToSound.isMajor(destination),sNr,dNr)
        val (a,b,c,d) = ChordBuilder1.buildSequence(LetterToSound.isMajor(source), LetterToSound.isMajor(destination),sNr,dNr)

        val t1 = a.zipWithIndex.map{case (m,i) => Event((i*rate.value).toLong, m) }
        val t2 = b.zipWithIndex.map{case (m,i) => Event((i*rate.value).toLong, m) }
        val t3 = c.zipWithIndex.map{case (m,i) => Event((i*rate.value).toLong, m) }
        val t4 = d.zipWithIndex.map{case (m,i) => Event((i*rate.value).toLong, m) }

        val c1 = Track(t1)
        val c2 = Track(t2)
        val c3 = Track(t3)
        val c4 = Track(t4)

        val seq = Sequence(Vector(c1,c2,c3,c4))
        seq.write(path2)
      }
      case ModulationType.Second => {
        sq = ChordBuilder2.buildSequenceForSecondType(LetterToSound.isMajor(source), LetterToSound.isMajor(destination),sNr,dNr)
      }
      case ModulationType.Third => {
        sq = ChordBuilder3.buildSequenceForThirdType(LetterToSound.isMajor(source), LetterToSound.isMajor(destination), sNr, dNr)
      }
    }

    val t = sq.zipWithIndex.map{case (m,i) => Event((i*0.5*rate.value).toLong, m) }
    val c = Track(t)
    val seq2 = Sequence(Vector(c))
    seq2.write(path)

  }
}
