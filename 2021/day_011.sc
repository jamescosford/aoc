// Scala 2.13.6

import scala.annotation.tailrec
import scala.io.Source

object stuff {

  type XY = (Int, Int)

  def neighbs(lx: Int, ly: Int)(xy: XY): IndexedSeq[XY] =
    (for {
      dx <- (-1 to 1)
      dy <- (-1 to 1)
    } yield (xy._1 + dx, xy._2 + dy)).filter(_ != xy).filter {
      case (x, y) => x >= 0 && x < lx && y >= 0 && y < ly
    }

  def step(
    neighbs: XY => IndexedSeq[XY],
    unPopped: Map[XY, Int],
    popped: Set[XY],
    updates: IndexedSeq[XY]
  ): (Map[XY, Int], Set[XY]) =
    updates.headOption match {
      case None => unPopped -> popped
      case Some(xy) if unPopped(xy) == 9 =>
        step(
          neighbs,
          unPopped - xy,
          popped + xy,
          updates.filter(_ != xy) ++ neighbs(xy).filter(!popped.contains(_))
        )
      case Some(xy) => step(neighbs, unPopped + (xy -> (unPopped(xy) + 1)), popped, updates.tail)
    }

}

@main
def go() {

  import stuff._

  val data = Source
    .fromFile("2021/day_011_input.txt")
    .getLines()
    .map(_.map(_.toString.toInt))
    .toIndexedSeq

  val ly = data.length
  val lx = data.head.length

  val is = for {
    x <- (0 until lx)
    y <- (0 until ly)
  } yield (x, y)

  val neighMap = is.map { i =>
    i -> neighbs(lx, ly)(i)
  }.toMap

  println(
    (0 until 100)
      .foldLeft[(Long, Map[XY, Int])]((0L, is.map(xy => xy -> data(xy._2)(xy._1)).toMap)) {
        case ((acc, state), i) =>
          val (unPopped, popped) = step(
            neighMap,
            state,
            Set.empty,
            is
          )
          (acc + popped.size) -> (unPopped ++ popped.map(xy => xy -> 0).toMap)
      }
      ._1
  )

  @tailrec def untilSync(
    acc: Long,
    state: Map[XY, Int],
    t: Int
  ): Int =
    step(neighMap, state, Set.empty, is) match {
      case (_, popped) if popped.size == lx * ly => t
      case (unPopped, popped) =>
        untilSync(
          acc + popped.size,
          unPopped ++ popped.map(xy => xy -> 0).toMap,
          t + 1
        )
    }

  println(untilSync(0L, is.map(xy => xy -> data(xy._2)(xy._1)).toMap, 1))
}
