// Scala 2.13.6
import scala.io.Source

object stuff {

  type P  = (Int, Int, Int)
  type PS = IndexedSeq[P]

  case class Group(bs: IndexedSeq[P], mhdists: Map[Int, Set[(Int, Int)]])

  val beaconRx = "(-?\\d+),(-?\\d+),(-?\\d+)".r

  def parse(i: Iterator[String]): IndexedSeq[PS] = {
    LazyList
      .unfold[PS, LazyList[String]](
        LazyList.from(i)
      ) {
        case ll if ll.isEmpty => None
        case ll =>
          val ps = ll.tail
            .map {
              case beaconRx(x, y, z) => Some((x.toInt, y.toInt, z.toInt))
              case _                 => None
            }
            .takeWhile(_.isDefined)
            .map(_.get)
            .toIndexedSeq
          Some(ps -> ll.splitAt(ps.length + 2)._2)
      }
      .toIndexedSeq
  }

  type Rot = (P, P, P)
  val rx90: Rot = (
    (1, 0, 0),
    (0, 0, -1),
    (0, 1, 0)
  )
  val ry90: Rot = (
    (0, 0, 1),
    (0, 1, 0),
    (-1, 0, 0)
  )
  val rz90: Rot = (
    (0, -1, 0),
    (1, 0, 0),
    (0, 0, 1)
  )

  val ident: Rot = (
    (1, 0, 0),
    (0, 1, 0),
    (0, 0, 1)
  )

  val test1: Rot = (
    (1, 2, 3),
    (4, 5, 6),
    (7, 8, 9)
  )

  def show(r: Rot): String =
    s"${r._1._1} ${r._1._2} ${r._1._3}" + System.lineSeparator() +
      s"${r._2._1} ${r._2._2} ${r._2._3}" + System.lineSeparator() +
      s"${r._3._1} ${r._3._2} ${r._3._3}"

  def tran(r: Rot): Rot = (
    (r._1._1, r._2._1, r._3._1),
    (r._1._2, r._2._2, r._3._2),
    (r._1._3, r._2._3, r._3._3)
  )

  def mult(r1: Rot, r2: Rot): Rot = {
    val r2t = tran(r2)
    (
      (dot(r1._1, r2t._1), dot(r1._1, r2t._2), dot(r1._1, r2t._3)),
      (dot(r1._2, r2t._1), dot(r1._2, r2t._2), dot(r1._2, r2t._3)),
      (dot(r1._3, r2t._1), dot(r1._3, r2t._2), dot(r1._3, r2t._3))
    )
  }

  def dot(a: P, b: P): Int =
    a._1 * b._1 + a._2 * b._2 + a._3 * b._3

  def rot(p: P, r: Rot): P =
    (dot(p, r._1), dot(p, r._2), dot(p, r._3))

  def selfRots(r: Rot) = LazyList.unfold[Rot, Rot](r) {
    case acc =>
      Some(acc -> mult(acc, r))
  }

  def allRots =
    (for {
      rx <- selfRots(rx90).take(4)
      ry <- selfRots(ry90).take(4)
      rz <- selfRots(rz90).take(4)
    } yield mult(mult(rx, ry), rz)).toSet.toSeq

  def manhDist(a: P, b: P): Int =
    math.abs(a._1 - b._1) +
      math.abs(a._2 - b._2) +
      math.abs(a._3 - b._3)
}

@main
def go() {

  import stuff._

  val lines = Source
    .fromFile("2021/day_019_input.txt")
    .getLines()

  val input = parse(lines).map {
    case ps =>
      val wi = ps.zipWithIndex
      val dists = (for {
        a <- wi
        b <- wi
      } yield (manhDist(a._1, b._1) -> (math.min(a._2, b._2) -> math.max(a._2, b._2))))
        .foldLeft(Map.empty[Int, Set[(Int, Int)]]) {
          case (acc, (d, _)) if d == 0 => acc
          case (acc, (d, ab))          => acc + (d -> (acc.get(d).getOrElse(Set.empty) + ab))
        }
      Group(ps, dists)
  }

  val testBs = IndexedSeq(
    (-1, -1, 1),
    (-2, -2, 2),
    (-3, -3, 3),
    (-2, -3, 1),
    (5, 6, -4),
    (8, 0, 7)
  )

  // (0 until 4).foldLeft(ident) {
  //   case (r, _) =>
  //     val rx = mult(r, rz90)
  //     testBs.map(rot(_, rx)).foreach(println)
  //     println
  //     rx
  // }

  // selfRots(rx90).take(4)
  // selfRots(ry90).take(4)
  // selfRots(rz90).take(4)

  // println(allRots.size)

  testBs
    .sliding(2)
    .map {
      case Seq(a, b, _*) => manhDist(a, b)
      case _             => -1
    }
    .foreach(println)

  input.zipWithIndex.foreach {
    case (Group(bs, mhs), i) =>
      println(s"Group $i")
      bs.foreach(println)
      mhs.toList.sortBy(_._1).foreach(println)
  }

  val seed = input.head
  input.tail
    .map {
      case g => g -> g.mhdists.keySet.intersect(seed.mhdists.keySet).size
    }
    .sortBy(_._2)
    .reverse
    .map(_._1)
    .take(1)
    .map { next =>
      val seedSingles = seed.mhdists.filter(_._2.size == 1).map(x => x._1 -> x._2.head)
      val nextSingles = next.mhdists.filter(_._2.size == 1).map(x => x._1 -> x._2.head)

      val common = seedSingles.keySet.intersect(nextSingles.keySet)
      // println(common.size)

      val pt = common
        .map { d =>
          d -> (seedSingles(d), nextSingles(d))
        }
        .toSeq
        .sortBy(_._1)
        .head

      allRots.foreach { r =>
        val p1a = seed.bs(pt._2._1._1)
        val p1b = seed.bs(pt._2._1._2)
        val p2a = rot(next.bs(pt._2._2._1), r)
        val p2b = rot(next.bs(pt._2._2._2), r)

        val da = manhDist(p1a, p2a)
        val db = manhDist(p1a, p2b)

        val dc = manhDist(p1b, p2a)
        val dd = manhDist(p1b, p2b)

        println(s"da: $da; db: $db; dc: $dc; dd: $dd;")

        val m = (((da == dc) || (da == dd)) && ((db == dc) || (db == dd)))
        if (m)
          println("boop")

      }
    }

}

/*



  set: registered vs unregistered

  select one group as the seed
    order the remaining groups by the size of intersection of the internal distances
    in descending order
      take group with the most mutual internal distances
      test all rotations on the set, until the manhattan distance between matching pairs is the same

      match the beacons
      transform all beacons in the new group to align the matching beacons;
      merge the groups





      for each rotation calculate the dist between all points; find a subset of 12 with the same dist


 */
