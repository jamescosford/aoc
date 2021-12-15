// Scala 2.13.6

import scala.util.matching.Regex
import scala.annotation.tailrec
import scala.io.Source

import scala.collection.immutable.SortedSet

object stuff {

  type XY = (Int, Int)
  case class Path(pos: XY, from: XY, cost: Int)
  type State = List[Path]
  type Grid  = IndexedSeq[IndexedSeq[Int]]

  def neighbs(g: Grid, pos: XY, from: XY): IndexedSeq[XY] =
    IndexedSeq(
      -1 -> 0,
      1  -> 0,
      0  -> -1,
      0  -> 1
    ).map(dxdy => (pos._1 + dxdy._1) -> (pos._2 + dxdy._2))
      .filter(xy => xy._1 >= 0 && xy._1 < g.head.length && xy._2 >= 0 && xy._2 < g.length)
      .filter(p => p != from && p != pos)

  def cheapestPath(g: Grid, s: State, m: Map[XY, Int]): Path = {
    val succs = neighbs(g, s.head.pos, s.head.from)
      .map {
        case (x, y) => Path(x -> y, s.head.pos, s.head.cost + g(y)(x))
      }
      .flatMap {
        case p @ Path(pos, _, cost) =>
          val min = m.get(pos).getOrElse(Int.MaxValue)
          Option.when(cost < min)(p)
      }
    val end = (g.head.length - 1) -> (g.length - 1)
    succs.filter(_.pos == end).sortBy(_.cost).headOption match {
      case None =>
        val newState = (s.tail ++ succs).sortBy(_.cost)
        cheapestPath(
          g,
          (s.tail ++ succs).sortBy(_.cost),
          m ++ succs.map(s => s.pos -> s.cost).toMap
        )
      case Some(end) => end
    }
  }

}

@main
def go() {

  import stuff._

  val grid = Source
    .fromFile("2021/day_015_input.txt")
    .getLines()
    .map(_.map(_.toString().toInt))
    .toIndexedSeq

  val res = cheapestPath(grid, List(Path(0 -> 0, 0 -> 0, 0)), Map.empty)

  println(res)
}
