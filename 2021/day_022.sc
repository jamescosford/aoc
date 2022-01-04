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
  def cut(a: Range, by: Range): Seq[Range] = {
    (a, by) match {
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
  }

  def cut(a: R3, by: R3): Set[R3] =
    (for {
      x <- cut(a.x, by.x)
      y <- cut(a.y, by.y)
      z <- cut(a.z, by.z)
    } yield R3(x, y, z)).filter {
      case R3(x, y, z) => x.nonEmpty && y.nonEmpty && z.nonEmpty
    }.toSet

  def doCut(a: R3, by: R3, oo: OnOff): Set[R3] = {
    val upd = cut(a, by)
      .filter { r3 =>
        oo match {
          case OnOff.On  => aContainsB(a, r3) || aContainsB(by, r3)
          case OnOff.Off => aContainsB(a, r3) && !intersect(by, r3)
        }
      }
    oo match {
      case OnOff.On  => assert(upd.toSeq.map(size).sum <= (size(a) + size(by)))
      case OnOff.Off => assert(upd.toSeq.map(size).sum <= size(a))
    }
    upd
  }

  def aContainsB(a: Range, b: Range): Boolean =
    b.start >= a.start && b.last <= a.last

  def aContainsB(a: R3, b: R3): Boolean =
    aContainsB(a.x, b.x) && aContainsB(a.y, b.y) && aContainsB(a.z, b.z)

  def intersect(a: Range, b: Range): Boolean =
    (a.contains(b.start) || a.contains(b.last) || b.contains(a.start) || b.contains(a.last))

  def intersect(a: R3, b: R3): Boolean =
    intersect(a.x, b.x) && intersect(a.y, b.y) && intersect(a.z, b.z)

  def size(r: R3): Long =
    r.x.size.toLong * r.y.size.toLong * r.z.size.toLong

  def onAfterReset(ll: LazyList[Line]): Long =
    LazyList
      .unfold[Long, (Set[R3], Seq[Line])]((Set.empty, ll)) {
        case (acc, l) =>
          l.headOption match {
            case None => None
            case Some(line) =>
              val szb4 = acc.toSeq.map(size).sum
              val upd =
                if (acc.isEmpty && line._1 == OnOff.On) Set(line._2)
                else
                  acc.flatMap(
                    i => doCut(i, line._2, line._1)
                  )

              upd.toSeq.sortBy(_.x.start).foreach(println)

              upd.foreach { r3 =>
                line._1 match {
                  case OnOff.On  => ()
                  case OnOff.Off => assert(!intersect(r3, line._2))
                }
              }
              val sz = upd.toSeq.map(size).sum
              println(s"b4: ${szb4}; after: ${sz}; delta: ${sz - szb4}")
              println
              Some(sz -> (upd, l.tail))
          }
      }
      .take(2)
      .last
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
      .map(x => { println(x); x })
  )

  val a = (10 to 12)
  val b = (11 to 13)

  cut(a, b).foreach(println)

  val la = parse("on x=10..12,y=10..12,z=10..12")
  val lb = parse("on x=11..13,y=11..13,z=11..13")
  val lc = parse("off x=9..11,y=9..11,z=9..11")
  val ld = parse("on x=10..10,y=10..10,z=10..10")

  // {
  //   assert(size(la._2) == 27)
  //   assert(size(lb._2) == 27)

  //   doCut(la._2, lb._2, OnOff.On).foreach(println)
  //   doCut(la._2, lb._2, OnOff.On).toSeq.map(size).foreach(println)
  //   println(doCut(la._2, lb._2, OnOff.On).toSeq.map(size).sum)
  //   assert(doCut(la._2, lb._2, OnOff.On).toSeq.map(size).sum - 27 == 19)
  // }
  // {
  //   val la = parse("on x=1..5,y=1..5,z=1..1")
  //   val lb = parse("on x=2..6,y=2..6,z=1..1")
  //   doCut(la._2, lb._2, OnOff.On).foreach(println)
  //   doCut(la._2, lb._2, OnOff.On).toSeq.map(size).foreach(println)
  //   println(doCut(la._2, lb._2, OnOff.On).toSeq.map(size).sum)

  //   println(size(la._2))
  //   println(size(lb._2))
  //   println(doCut(la._2, lb._2, OnOff.On).toSeq.map(size).sum)
  //   assert(doCut(la._2, lb._2, OnOff.On).toSeq.map(size).sum == 34)

  //   println(size(la._2))
  //   println(size(lb._2))
  //   println(doCut(la._2, lb._2, OnOff.Off).toSeq.map(size).sum)
  //   assert(doCut(la._2, lb._2, OnOff.Off).toSeq.map(size).sum == 9)
  // }
  // {
  //   val la = parse("on x=1..5,y=1..5,z=1..1")
  //   val lb = parse("off x=2..6,y=2..6,z=1..1")

  //   println(onAfterReset(LazyList.from(List(la, lb))))
  // }

  // {
  //   val la = parse("on x=1..10,y=1..10,z=1..10")
  //   val lb = parse("off x=6..15,y=6..15,z=6..10")
  //   val lc = parse("on x=6..10,y=6..10,z=6..6")

  //   println(onAfterReset(LazyList.from(List(la, lb, lc))))
  // }

  // {
  //   val la = parse("on x=-4..5,y=-4..5,z=1..10")
  //   val lb = parse("off x=-2..2,y=-2..2,z=6..10")
  //   val lc = parse("on x=6..10,y=6..10,z=6..6")

  //   println(onAfterReset(LazyList.from(List(la, lb, lc))))
  // }

  // {
  //   val la = parse("on x=-10..-1,y=-10..-1,z=-10..-1")
  //   val lb = parse("off x=-2..2,y=-2..2,z=-5..-1")
  //   val lc = parse("on x=6..10,y=6..10,z=6..6")

  //   println(onAfterReset(LazyList.from(List(la, lb, lc))))
  // }

  {
    val la = parse("on x=-20..26,y=-36..17,z=-47..7")
    val lb = parse("on x=-20..33,y=-21..23,z=-26..28")
    val lx = parse("on x=27..33,y=18..23,z=8..28")

    println(size(la._2))
    println(size(lb._2))
    println(size(lx._2))

    println(onAfterReset(LazyList.from(List(la, lb))))
  }
//  val sz = onAfterReset(lines)

  // println(sz)
}
