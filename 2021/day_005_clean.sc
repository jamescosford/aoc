// Scala 2.13.6

import scala.io.Source
import scala.annotation.tailrec

object stuff {

  type VentMap = Map[(Int, Int), Int]

  type Vent = ((Int, Int), (Int, Int))

  val ventRx = raw"(\d+),(\d+) -> (\d+),(\d+)".r
  def readLine(l: String): Vent = {
    l match {
      case ventRx(x1, y1, x2, y2) => (x1.toInt, y1.toInt) -> (x2.toInt, y2.toInt)
      case _                      => ???
    }
  }

  def isAxisAligned(v: Vent): Boolean =
    v match {
      case ((x1, y1), (x2, y2)) => x1 == x2 || y1 == y2
    }

  def rangeFromPair(_1: Int, _2: Int): Range =
    math.signum(_2 - _1) match {
      case 0 => Range.apply(_1, _2)
      case x => Range.inclusive(_1, _2, x)
    }

  def enumerate(v: Vent): List[(Int, Int)] =
    v match {
      case ((x1, y1), (x2, y2)) =>
        rangeFromPair(x1, x2).toList.zipAll(rangeFromPair(y1, y2).toList, x1, y1)
    }

}

@main
def go() {

  import stuff._

  val data = Source
    .fromFile("2021/day_005_input.txt")
    .getLines()
    .map(stuff.readLine)
    .filter(stuff.isAxisAligned)
    .map(enumerate)
    .foldLeft[VentMap](Map.empty) {
      case (vm, vent) =>
        val toAdd = vent.zip(vent.map(vm.get)).map {
          case (p, hits) => p -> (hits.getOrElse(0) + 1)
        }
        vm ++ toAdd
    }
    .toList
    .filter(_._2 > 1)
    .length

  println(data)

  val result2 = Source
    .fromFile("2021/day_005_input.txt")
    .getLines()
    .map(readLine)
    .map(enumerate)
    .foldLeft[VentMap](Map.empty) {
      case (vm, vent) =>
        vm ++ vent.zip(vent.map(vm.get)).map {
          case (p, hits) => p -> (hits.getOrElse(0) + 1)
        }
    }
    .toList
    .filter(_._2 > 1)
    .length

  println(result2)
}
