// Scala 2.13.6

import scala.io.Source
import scala.annotation.tailrec

object stuff {

  def costTo(x: Int, cs: List[Int]): Long =
    cs.foldLeft(0L) {
      case (tot, c) => tot + math.abs(x - c)
    }

  def costTo2(x: Int, cs: List[Int]): Long =
    cs.foldLeft(0L) {
      case (tot, c) => tot + (0 to math.abs(x - c)).sum
    }

}

@main
def go() {

  import stuff._

  val data = Source
    .fromFile("2021/day_007_input.txt")
    .getLines()
    .next()
    .split(',')
    .map(_.toInt)
    .toList
    .sorted

  {

    val avg = data.sum.toDouble / data.length.toDouble
    val med = data(data.length / 2)

    println(s"avg: $avg")

    val costToAvg = costTo(avg.toInt, data)
    val costToMed = costTo(med, data)

    println(costTo2(0, List(1, 2, 3)))

    println(s"cost to avg: $costToAvg")
    println(s"cost to med: $costToMed")
  }

  {

    // val data = List(16, 1, 2, 0, 4, 2, 7, 1, 2, 14).sorted
    val avg = data.sum.toDouble / data.length.toDouble
    val med = data(data.length / 2)

    println(s"avg: $avg")
    println(s"avg.toInt: ${avg.toInt}")
    println(s"avg.round.toInt: ${avg.round.toInt}")

    val costToAvg = costTo2(avg.round.toInt, data)
    val costToMed = costTo2(med, data)

    println(s"cost to avg: $costToAvg")
    println(s"cost to med: $costToMed")
  }
}
