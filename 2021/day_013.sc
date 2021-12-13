// Scala 2.13.6

import scala.util.matching.Regex
import scala.annotation.tailrec
import scala.io.Source

object stuff {
  val dotRx  = "(\\d+),(\\d+)".r
  val foldRx = "fold along ([xy])=(\\d+)".r

  type XY     = (Int, Int)
  type Points = Set[XY]

  sealed trait FoldDim
  case object AlongX extends FoldDim
  case object AlongY extends FoldDim

  case class Fold(dim: FoldDim, i: Int)

  def parse(s: String): Option[Either[XY, Fold]] =
    s match {
      case dotRx(x, y) => Some(Left(x.toInt -> y.toInt))
      case foldRx(dim, i) =>
        Some(Right(Fold(dim match {
          case "x" => AlongX
          case "y" => AlongY
          case _   => ???
        }, i.toInt)))
      case _ => None
    }

  def foldPoints(points: Set[XY], fold: Fold): Set[XY] =
    points.map {
      case (x, y) =>
        fold.dim match {
          case AlongY if y > fold.i => (x, fold.i - (y - fold.i))
          case AlongX if x > fold.i => (fold.i - (x - fold.i), y)
          case _                    => (x, y)
        }
    }

  def toLine(y: Int, points: Set[XY]): String = {
    val toPrint  = points.filter(_._2 == y).toList.sortBy(_._1)
    val toPrintS = toPrint.toSet
    if (toPrint.isEmpty)
      ""
    else {
      (0 to toPrint.last._1).map { x =>
        if (toPrintS.contains(x -> y)) "X" else " "
      }.mkString
    }
  }
}

@main
def go() {

  import stuff._

  val data = Source
    .fromFile("2021/day_013_input.txt")
    .getLines()
    .map(parse)
    .toList
    .filter(_.isDefined)
    .map(_.get)

  val points = data.filter(_.isLeft).map(_.swap.toOption.get).toSet
  val extents = points.foldLeft[XY](0 -> 0) {
    case (bb, xy) =>
      (math.max(bb._1, xy._1), math.max(bb._2, xy._2))
  }

  val folds = data.filter(_.isRight).map(_.toOption.get)

  val res = folds.foldLeft[Points](points) {
    case (pts, fold) => foldPoints(pts, fold)
  }

  val yext = res
    .foldLeft[XY](0 -> 0) {
      case (bb, xy) =>
        (math.max(bb._1, xy._1), math.max(bb._2, xy._2))
    }
    ._2
  (0 to yext)
    .map { y =>
      toLine(y, res)
    }
    .foreach(println)
}
