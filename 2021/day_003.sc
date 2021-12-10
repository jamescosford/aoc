// Scala 2.13.6

import scala.io.Source
import scala.annotation.tailrec

@main
def go() {

  sealed abstract class Command(delta: Int)
  object Command {
    case class Forward(delta: Int) extends Command(delta)
    case class Down(delta: Int)    extends Command(delta)
    case class Up(delta: Int)      extends Command(delta)

    val forward = "forward ([0-9]+)".r
    val down    = "down ([0-9]+)".r
    val up      = "up ([0-9]+)".r

    def lineToCommand(s: String): Command = s match {
      case forward(delta) => Forward(delta.toInt)
      case down(delta)    => Down(delta.toInt)
      case up(delta)      => Up(delta.toInt)
    }
  }

  def tally(s: String): Seq[Int] =
    s.map { c =>
      if (c == '1') 1 else 0
    }

  val data = Source
    .fromFile("2021/day_003_input.txt")
    .getLines()
    .toIndexedSeq

  println(
    Option(
      data
        .foldLeft[(Option[Seq[Int]], Int)](None -> 0) {
          case ((None, n), s) => (Some(tally(s)), n + 1)
          case ((acc, n), s)  => (acc.map { _.zip(tally(s)).map(t => t._1 + t._2) }, n + 1)
        }
    ).map {
      case (tally, n) =>
        val gammaString = tally.get.map {
          case ones if ones > n / 2 => '1'
          case _                    => '0'
        }.mkString
        val gamma = Integer.parseInt(gammaString, 2)
        val epsilonString = gammaString.map { c =>
          if (c == '1') '0' else '1'
        }.mkString
        val epsilon = Integer.parseInt(epsilonString, 2)
        gamma * epsilon
    }
  )

  def applyBitCriteria(
    criteria: (Int, Int) => Char,
    index: Int,
    remainder: IndexedSeq[String]
  ): String = remainder match {
    case Seq(res) => res
    case Seq()    => ???
    case _ =>
      val ones = remainder.foldLeft(0) {
        case (acc, s) => acc + (if (s.charAt(index) == '1') 1 else 0)
      }
      val toKeep = criteria(ones, remainder.size)
      applyBitCriteria(criteria, index + 1, remainder.filter(_.charAt(index) == toKeep))
  }

  val firstCriteria: (Int, Int) => Char = (ones: Int, size: Int) => {
    if (ones >= (size / 2)) '1' else '0'
  }
  val secondCriteria: (Int, Int) => Char = (ones: Int, size: Int) => {
    if (ones < (size / 2)) '1' else '0'
  }

  val o2String = applyBitCriteria(
    firstCriteria,
    0,
    data
  )
  val o2 = Integer.parseInt(o2String, 2)
  val co2String = applyBitCriteria(
    secondCriteria,
    0,
    data
  )
  val co2 = Integer.parseInt(co2String, 2)
  println(s"$o2String => $o2")
  println(s"$co2String => $co2")
  println(o2 * co2)

}
