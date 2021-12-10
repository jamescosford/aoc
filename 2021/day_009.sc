// Scala 2.13.6

import scala.annotation.tailrec

import scala.io.Source

object stuff {

  type HMap = IndexedSeq[IndexedSeq[Int]]

  val neighs = List(
    (-1, 0),
    (1, 0),
    (0, -1),
    (0, 1)
  )

  def neigh(map: HMap)(_x: Int, _y: Int): List[Int] =
    neighs.map {
      case (dx, dy) =>
        val x = _x + dx
        val y = _y + dy
        Option.when(x >= 0 && x < map(0).length && y >= 0 && y < map.length) {
          map(y)(x)
        }
    }.flatten

  def lowPoints(map: HMap): IndexedSeq[(Int, Int)] =
    (for {
      y <- (0 until map.length)
      x <- (0 until map(0).length)
    } yield (x, y)).flatMap {
      case (x, y) =>
        val nbs = neigh(map)(x, y)
        val v   = map(y)(x)
        Option.when(nbs.min > v) { x -> y }
    }

  def visit(map: HMap, seen: Set[(Int, Int)])(_x: Int, _y: Int): List[(Int, Int)] =
    neighs
      .map {
        case (dx, dy) => (_x + dx, _y + dy)
      }
      .filter {
        case (x, y) => x >= 0 && x < map(0).length && y >= 0 && y < map.length
      }
      .filter(i => !seen.contains(i))

  @tailrec
  def grow(map: HMap, seen: Set[(Int, Int)], toSee: Set[(Int, Int)]): Set[(Int, Int)] =
    toSee.headOption match {
      case None => seen
      case Some(p @ (x, y)) =>
        val newToSee = visit(map, seen)(x, y).filter {
          case (x, y) => map(y)(x) != 9
        }.toSet
        grow(map, seen + p, newToSee ++ toSee - p)
    }

  def basinSize(map: HMap)(x: Int, y: Int): Int =
    grow(map, Set.empty, Set(x -> y)).filter {
      case (x, y) => map(y)(x) != 9
    }.size

}

@main
def go() {

  import stuff._

  val data = Source
    .fromFile("2021/day_10_input.txt")
    .getLines()
    .map(_.toCharArray().toIndexedSeq.map(_.toString()).map(_.toInt))
    .toIndexedSeq

  val q1Res = lowPoints(data).map {
    case (x, y) => 1 + data(y)(x)
  }.sum
  println(q1Res)

  val q2Res = lowPoints(data)
    .map {
      case (x, y) => basinSize(data)(x, y)
    }
    .sorted
    .reverse
    .take(3)
    .reduce(_ * _)

  println(q2Res)

}
