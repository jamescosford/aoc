// Scala 2.13.6

import scala.annotation.tailrec
import scala.io.Source

object stuff {

  sealed trait Cave {
    val name: String
  }
  object Cave {
    case object Start extends Cave {
      val name = "start"
    }
    case object End extends Cave {
      val name = "end"
    }
    case class Small(name: String) extends Cave
    case class Big(name: String)   extends Cave

    def fromString(s: String): Cave =
      s match {
        case "start"                   => Start
        case "end"                     => End
        case s if s.toLowerCase() == s => Small(s)
        case s if s.toUpperCase() == s => Big(s)
        case _                         => ???
      }
  }

  type Tunnel = (Cave, Cave)
  case class Path(path: IndexedSeq[Cave], spent: Set[Cave.Small], double: Option[Cave.Small])

  val rx = "([a-zA-Z]+)-([a-zA-Z]+)".r

  def parse(line: String): Tunnel =
    line match {
      case rx(ca, cb) => (Cave.fromString(ca), Cave.fromString(cb))
    }

  def sort(ts: List[Tunnel]): Map[Cave, List[Cave]] = {
    ts.flatMap(x => List(x._1, x._2))
      .toSet
      .foldLeft[Map[Cave, List[Cave]]](Map.empty) {
        case (res, cave) =>
          val forCave = ts
            .flatMap {
              case (a, b) if a == cave => Some(b)
              case (a, b) if b == cave => Some(a)
              case _                   => None
            }
            .filter(_ != Cave.Start)
          res + (cave -> forCave)
      }
  }

  @tailrec def findAllPaths(
    map: Map[Cave, List[Cave]],
    paths: IndexedSeq[Path],
    complete: IndexedSeq[Path]
  ): IndexedSeq[Path] =
    if (paths.isEmpty)
      return complete
    else {
      val path = paths.head
      val newPaths = map(path.path.last)
        .filter {
          case s: Cave.Small => !(path.spent.contains(s) && path.double.isDefined)
          case _             => true
        }
        .map { c =>
          path.copy(
            path = path.path :+ c,
            spent = c match {
              case c: Cave.Small => path.spent + c
              case _             => path.spent
            },
            double = c match {
              case c: Cave.Small if path.spent.contains(c) => Some(c)
              case _                                       => path.double
            }
          )
        }
      val comp   = newPaths.filter(_.path.last == Cave.End).toIndexedSeq
      val incomp = newPaths.filter(_.path.last != Cave.End).toIndexedSeq
      findAllPaths(map, paths.tail ++ incomp, complete ++ comp)
    }
}

@main
def go() {

  import stuff._

  val data = Source
    .fromFile("2021/day_012_input.txt")
    .getLines()
    .map(parse)
    .toList
  val sorted = sort(data)

  val result1 =
    findAllPaths(
      sorted,
      IndexedSeq(Path(IndexedSeq(Cave.Start), Set.empty, Some(Cave.Small("Dummy")))),
      IndexedSeq.empty
    )
  println(result1.length)

  val result2 =
    findAllPaths(
      sorted,
      IndexedSeq(Path(IndexedSeq(Cave.Start), Set.empty, None)),
      IndexedSeq.empty
    )
  println(result2.length)
}
