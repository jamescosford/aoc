// Ammonite 2.4.0, Scala 2.13.6

import scala.io.Source

object stuff {

  sealed trait OnOff
  object OnOff {
    case object On  extends OnOff
    case object Off extends OnOff
  }

  case class R3(
    x: Range,
    y: Range,
    z: Range
  )

  type Line = (OnOff, R3)

  def parse(s: String): Line = {
    val rn = raw"(-?\d+)\.\.(-?\d+)"
    val rx = s"^(on|off) x=$rn,y=$rn,z=$rn".r
    s match {
      case rx(oo, x1, x2, y1, y2, z1, z2) =>
        (oo match {
          case "on"  => OnOff.On
          case "off" => OnOff.Off
        }) -> R3(
          (x1.toInt to x2.toInt),
          (y1.toInt to y2.toInt),
          (z1.toInt to z2.toInt)
        )
    }
  }

  def cut(a: Range, by: Range): Seq[Range] = (a, by) match {
    case (a, by) if a.contains(by.start) && by.last >= a.last =>
      Seq((a.start until by.start), (by.start to a.last)) ++ Option.when(a.last + 1 <= by.last)(
        a.last + 1 to by.last
      )

    case (a, by) if a.contains(by.last) && by.start <= a.start =>
      Seq((by.start until a.start), (a.start to by.last)) ++ Option.when(by.last + 1 <= a.last)(
        by.last + 1 to a.last
      )

    case (a, by) if a.contains(by.start) && a.contains(by.last) =>
      Seq((a.start until by.start), by) ++ Option.when(by.last + 1 <= a.last)(
        by.last + 1 to a.last
      )

    case (a, by) if by.contains(a.start) && by.contains(a.last) =>
      Seq((by.start until a.start), (a.start to a.last)) ++ Option.when(a.last + 1 <= by.last)(
        a.last + 1 to by.last
      )

    case (a, by) => Seq(a, by)
  }

  def cut(a: R3, by: R3): Seq[R3] = {
    val xs = cut(a.x, by.x)
    val ys = cut(a.y, by.y)
    val zs = cut(a.z, by.z)
    (for {
      x <- xs
      y <- ys
      z <- zs
    } yield R3(x, y, z))
      .filter {
        case R3(x, y, z) => x.nonEmpty && y.nonEmpty && z.nonEmpty
      }
      .filter {
        case r3 => aContainsB(a, r3) && !aContainsB(by, r3)
      }
  }

  def intersect(a: Range, b: Range): Boolean =
    (a.contains(b.start) || a.contains(b.last) || b.contains(a.start) || b.contains(a.last))

  def intersect(a: R3, b: R3): Boolean =
    intersect(a.x, b.x) && intersect(a.y, b.y) && intersect(a.z, b.z)

  def aContainsB(a: Range, b: Range): Boolean =
    b.start >= a.start && b.last <= a.last

  def aContainsB(a: R3, b: R3): Boolean =
    aContainsB(a.x, b.x) && aContainsB(a.y, b.y) && aContainsB(a.z, b.z)

  def size(r: R3): Long =
    r.x.size.toLong * r.y.size.toLong * r.z.size.toLong

  def onAfterReset(ll: LazyList[Line]): Long = {
    ll.foldLeft(Seq.empty[R3]) {
        case (acc, l) =>
          if (acc.isEmpty && l._1 == OnOff.On) Seq(l._2)
          else {
            val in  = acc.filter(intersect(_, l._2))
            val out = acc.filter(!in.contains(_))
            out ++ in.flatMap(
              i => cut(i, l._2)
            ) ++ Option.when(l._1 == OnOff.On)(l._2)
          }
      }
      .map(size)
      .sum
  }
}

@main
def run() = {
  import stuff._

  println(
    onAfterReset(
      LazyList.from(
        Source
          .fromFile("2021/day_022_input.txt")
          .getLines()
          .map(parse)
      )
    )
  )

}
