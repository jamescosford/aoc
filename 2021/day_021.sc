// Scala 2.13.6

import scala.io.Source

object stuff {

  type Player = (BigInt, Int, Int)

  case class GameState(
    p1: Player,
    p2: Player,
    dice: BigInt = 0L,
    rolls: Int = 0,
    turn: Int = 1
  )

  case class P(dist: BigInt, pos: Int, score: Int)
  case class Id(p1: P, p2: P, turn: Int)
  type Dirac = Map[Id, BigInt]

  val successors = (for {
    x <- (1L to 3L)
    y <- (1L to 3L)
    z <- (1L to 3L)
  } yield x + y + z)
    .map(BigInt.apply)
    .foldLeft(Map.empty[BigInt, Int]) {
      case (acc, v) => acc + (v -> (acc.get(v).getOrElse(0) + 1))
    }
    .toList

  def updatePDirac(p: P, jump: BigInt): P = {
    val dist = p.dist + jump
    val pos  = (dist - 1) % 10 + 1
    P(
      dist = dist,
      pos = pos.toInt,
      score = p.score + pos.toInt
    )
  }

  // Step over all successor states and provide the set of next states.
  def stepId(id: Id): Seq[(Id, Int)] = successors.map {
    case (jump, n) =>
      id.turn match {
        case 1 => id.copy(p1 = updatePDirac(id.p1, jump), turn = 2) -> n
        case 2 => id.copy(p2 = updatePDirac(id.p2, jump), turn = 1) -> n
      }
  }

  def stepDirac(d: Dirac): Dirac =
    d.foldLeft(Map.empty[Id, BigInt]) {
      case (acc, (id, nIds)) =>
        stepId(id)
          .map {
            case (id, nPerId) => id -> (nIds * nPerId)
          }
          .foldLeft(acc) {
            case (acc, (id, n)) =>
              acc + (id -> (acc.get(id).getOrElse(BigInt.apply(0)) + n))
          }
    }

  def judgeDirac(d: Dirac) = {

    val nstates    = d.size
    val states     = d.map(_._2).sum
    val p1Wins     = d.filter(_._1.p1.score >= 21).map(_._2).sum
    val p2Wins     = d.filter(_._1.p2.score >= 21).map(_._2).sum
    val p1TopScore = d.map(_._1.p1.score).max
    val p2TopScore = d.map(_._1.p2.score).max

    // val p1Wins = p1.filter(_._1.score >= 21).map(_._2).sum
    // val p2Wins = p2.filter(_._1.score >= 21).map(_._2).sum

    // val p1TopScore = p1.map(_._1.score).max
    // val p2TopScore = p2.map(_._1.score).max
    val completed = d.filter(p => math.max(p._1.p1.score, p._1.p2.score) < 21).size == 0
    val p1Victory = p1Wins > states / 2
    val p2Victory = p2Wins > states / 2
    val finished  = p1Victory || p2Victory
    println(s"unique states: $nstates")
    println(
      s"total games: $states; finished: $finished; p1Victory: $p1Victory; p2Victory: $p2Victory"
    )
    println(s"p1 topscore: $p1TopScore; p1 wins: $p1Wins")
    println(s"p2 topscore: $p2TopScore; p2 wins: $p2Wins")

  }

  def goDirac = {
    val initState = Map[Id, BigInt](
      // Id(P(8L, 8, 0), P(3L, 3, 9), 1) -> 1L
      Id(P(4L, 0, 4), P(8L, 0, 8), 1) -> 1L
    )
    (0 until 9).foldLeft(initState) {
      case (ds, _) =>
        val d = stepDirac(ds)
        judgeDirac(d)
        d
    }
  }
}

@main def go() {
  import stuff._

  val sps = (8, 3)

  val initState = GameState(
    p1 = (8L, 0, 8),
    p2 = (3L, 0, 3)
  )

  // val initState = GameState(
  //   p1 = (4L, 0, 4),
  //   p2 = (8L, 0, 8)
  // )

  // println(res1)
  // println(math.min(res1._1._1, res1._1._2) * res1._2)

  goDirac

}
