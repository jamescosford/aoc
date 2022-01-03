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
    val rn = raw"(\d+)\.\.(\d+)"
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

  /*
    (1)
    a ********
    b     *******
    = ****
          ****
              ***

    (2)
    a    *****
    b ******
    =       **
         ***
      ***
    (3)
    a **********
    b     ****
    = ****    **
          ****

    (4)
    a    ****
    b **********
    =
      **********
   */
  def cut(a: Range, by: Range, oo: OnOff): Seq[Range] = {
    (a, by) match {
      case (a, by) if a.contains(by.start) && by.last >= a.last =>
        oo match {
          case OnOff.On =>
            Seq((a.start until by.start), (by.start to a.last), (a.last + 1 to by.last))
          case OnOff.Off => Seq((a.start until by.start))
        }

      case (a, by) if a.contains(by.last) && by.start <= a.start =>
        oo match {
          case OnOff.On =>
            Seq((by.start until a.start), (a.start to by.last), (by.last + 1 to a.last))
          case OnOff.Off => Seq((by.last + 1 to a.last))
        }

      case (a, by) if a.contains(by.start) && a.contains(by.last) =>
        oo match {
          case OnOff.On  => Seq((a.start until by.start), by, (by.last + 1 to a.last))
          case OnOff.Off => Seq((a.start until by.start), (by.last + 1 to a.last))
        }

      case (a, by) if by.contains(a.start) && by.contains(a.last) =>
        oo match {
          case OnOff.On  => Seq(by)
          case OnOff.Off => Seq.empty
        }

      case (a, _) => Seq(a)
    }
  }

  def cut(a: R3, by: R3, oo: OnOff): Set[R3] =
    (for {
      x <- cut(a.x, by.x, oo)
      y <- cut(a.y, by.y, oo)
      z <- cut(a.z, by.z, oo)
    } yield R3(x, y, z)).filter {
      case R3(x, y, z) => x.nonEmpty && y.nonEmpty && z.nonEmpty
    }.toSet

  def doCut(a: R3, by: R3, oo: OnOff): Set[R3] =
    cut(a, by, oo)
      .filter {
        case r3 =>
          oo match {
            case OnOff.On  => intersect(a, r3) || intersect(by, r3)
            case OnOff.Off => intersect(a, r3) && !intersect(by, r3)
          }
      }

  def intersect(a: Range, b: Range): Boolean =
    (a.contains(b.start) || a.contains(b.last) || b.contains(a.start) || b.contains(a.last))

  def intersect(a: R3, b: R3): Boolean =
    intersect(a.x, b.x) && intersect(a.y, b.y) && intersect(a.z, b.z)

  def size(r: R3): Long =
    r.x.size.toLong * r.y.size.toLong * r.z.size.toLong

}

@main
def run() = {
  import stuff._

  val lines = LazyList.from(
    Source
      .fromFile("2021/day_022_test_input.txt")
      // .fromFile("day_022_input.txt")
      .getLines()
      .map(parse)
  )

  val a = (10 to 12)
  val b = (11 to 13)

  cut(a, b, OnOff.Off).foreach(println)

  val la = parse("on x=10..12,y=10..12,z=10..12")
  val lb = parse("on x=11..13,y=11..13,z=11..13")
  val lc = parse("off x=9..11,y=9..11,z=9..11")
  val ld = parse("on x=10..10,y=10..10,z=10..10")

  {
    assert(size(la._2) == 27)
    assert(size(lb._2) == 27)

    doCut(la._2, lb._2, OnOff.On).foreach(println)
    doCut(la._2, lb._2, OnOff.On).toSeq.map(size).foreach(println)
    println(doCut(la._2, lb._2, OnOff.On).toSeq.map(size).sum)
    assert(doCut(la._2, lb._2, OnOff.On).toSeq.map(size).sum - 27 == 19)
  }
  {
    val la = parse("on x=1..5,y=1..5,z=1..1")
    val lb = parse("on x=2..6,y=2..6,z=1..1")
    doCut(la._2, lb._2, OnOff.On).foreach(println)
    doCut(la._2, lb._2, OnOff.On).toSeq.map(size).foreach(println)
    println(doCut(la._2, lb._2, OnOff.On).toSeq.map(size).sum)

    println(size(la._2))
    println(size(lb._2))
    println(doCut(la._2, lb._2, OnOff.On).toSeq.map(size).sum)
    assert(doCut(la._2, lb._2, OnOff.On).toSeq.map(size).sum == 34)

    println(size(la._2))
    println(size(lb._2))
    println(doCut(la._2, lb._2, OnOff.Off).toSeq.map(size).sum)
    assert(doCut(la._2, lb._2, OnOff.Off).toSeq.map(size).sum == 9)
  }

  val sz = LazyList
    .unfold[Long, (Set[R3], Seq[Line])]((Set(lines.head._2), lines.tail)) {
      case (acc, l) =>
        println(l.headOption)
        l.headOption match {
          case Some(line) =>
            val upd = acc.flatMap(
              i => doCut(i, line._2, line._1)
            )
            val sz = upd.toSeq.map(size).sum
            Some(sz -> (upd, l.tail))
          case None => None
        }
    }
    .map(x => { println(s"on: $x"); x })
    .toList

  // println(sz)
}
