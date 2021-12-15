// Scala 2.13.6

import scala.util.matching.Regex
import scala.annotation.tailrec
import scala.io.Source

import scala.collection.immutable.SortedSet

object stuff {

  type GC  = (Int, Int)
  type Pos = (GC, GC)
  case class Path(pos: Pos, from: Pos, cost: Int)
  type State = List[Path]
  case class Grid(
    g: IndexedSeq[IndexedSeq[Int]],
    rx: Int,
    ry: Int
  )

  def stepCoord(p: GC, gmax: Int, step: Int): GC =
    p match {
      case (tx, px) if px + step > gmax => (tx + 1, 0)
      case (tx, px) if px + step < 0    => (tx - 1, gmax)
      case (tx, px)                     => (tx, px + step)
    }

  def neighbs(g: Grid, pos: Pos, from: Pos): IndexedSeq[Pos] =
    IndexedSeq(
      -1 -> 0,
      1  -> 0,
      0  -> -1,
      0  -> 1
    ).map { dxdy =>
        val (gx, gy) = pos
        (
          stepCoord(gx, g.g.head.length - 1, dxdy._1),
          stepCoord(gy, g.g.length - 1, dxdy._2)
        )
      }
      .filter {
        case (px, py) => px._1 >= 0 && px._1 < g.rx && py._1 >= 0 && py._1 < g.ry
      }
      .filter(p => p != from && p != pos)

  def cost(g: Grid, p: Pos): Int = {
    val base = g.g(p._2._2)(p._1._2)
    val cx   = p._1._1
    val cy   = p._2._1
    val tx   = base + cx + cy
    if (tx > 9)
      tx % 9
    else tx
  }

  def cheapestPath(g: Grid, s: State, m: Map[Pos, Int]): Path = {
    val succs = neighbs(g, s.head.pos, s.head.from)
      .map {
        case p @ (x, y) => Path(x -> y, s.head.pos, s.head.cost + cost(g, p))
      }
      .flatMap {
        case p @ Path(pos, _, cost) =>
          val min = m.get(pos).getOrElse(Int.MaxValue)
          Option.when(cost < min)(p)
      }
    val end = (g.rx - 1, g.g.head.length - 1) -> (g.ry - 1, g.g.length - 1)
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

  val start = (0 -> 0) -> (0 -> 0)

  val g1   = Grid(grid, 1, 1)
  val res1 = cheapestPath(g1, List(Path(start, start, 0)), Map.empty)
  println(res1.cost)

  val g2   = Grid(grid, 5, 5)
  val res2 = cheapestPath(g2, List(Path(start, start, 0)), Map.empty)
  println(res2.cost)
}
