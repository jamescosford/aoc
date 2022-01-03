import fastparse.internal.Lazy
// Scala 2.13.6

import scala.io.Source

object stuff {

  case class P(dist: BigInt, pos: Int, score: Int)
  case class Id(p1: P, p2: P, turn: Int, n: BigInt)
  type Dirac = (Seq[Id], (BigInt, BigInt))

  def Q1(initState: Id): Long = {

    val q1Rolls = LazyList
      .unfold[Int, Int](1) {
        case s if s == 100 => Some(s -> 1)
        case s             => Some(s -> (s + 1))
      }
      .zipWithIndex
      .map(x => x._1 -> (x._2 + 1))

    val q1Turns = LazyList.unfold[(Int, Int), LazyList[(Int, Int)]](q1Rolls) {
      case rolls =>
        val (thisRoll, rem) = rolls.splitAt(3)
        Some((thisRoll.map(_._1).sum -> (thisRoll.last._2)) -> rem)
    }

    LazyList
      .unfold[(Id, Int), (Id, LazyList[(Int, Int)])](initState -> q1Turns) {
        case (id, turns) =>
          val stepped = stepId(id, turns.head._1, 1)
          Some((stepped, turns.head._2) -> (stepped -> turns.tail))
      }
      .dropWhile(id => math.max(id._1.p1.score, id._1.p2.score) < 1000)
      .map {
        case (id, rolls) => math.min(id.p1.score, id.p2.score) * rolls
      }
      .head
  }

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

  def stepId(id: Id, jump: BigInt, n: Int): Id =
    id.turn match {
      case 1 => id.copy(p1 = updatePDirac(id.p1, jump), turn = 2, n = id.n * n)
      case 2 => id.copy(p2 = updatePDirac(id.p2, jump), turn = 1, n = id.n * n)
    }

  // Step over all successor states and provide the set of next states.
  def stepId(id: Id): Either[Seq[Id], (BigInt, BigInt)] =
    math.max(id.p1.score, id.p2.score) match {
      case s if s >= 21 =>
        Right(
          if (id.p1.score > id.p2.score)
            id.n -> BigInt(0)
          else
            BigInt(0) -> id.n
        )
      case _ =>
        Left {
          successors.map { case (jump, n) => stepId(id, jump, n) }
        }
    }

  def stepDirac(d: Dirac): Dirac = {
    val stepped = d._1.map(stepId(_))
    val conts   = stepped.filter(_.isLeft).map(_.swap.toOption.get)
    val wins = stepped.filter(_.isRight).map(_.toOption.get).foldLeft(d._2) {
      case ((a1, a2), (p1, p2)) => (a1 + p1) -> (a2 + p2)
    }
    conts.flatten -> wins
  }

  def judgeDirac(d: Dirac) = {
    val nstates   = d._1.size
    val completed = d._1.size == 0
    val p1Wins    = d._2._1
    val p2Wins    = d._2._2
    println(s"unique states: $nstates")
    println(s"p1 wins: $p1Wins")
    println(s"p2 wins: $p2Wins")
  }

  def goDirac = {
    val initState = Seq(
      Id(P(8L, 8, 0), P(3L, 3, 0), 1, BigInt(1L))
      // Id(P(4L, 4, 0), P(8L, 8, 0), 1, BigInt(1L))
    )
    (0 until 20).foldLeft(initState -> (BigInt(0), BigInt(0))) {
      case (ds, _) =>
        val d = stepDirac(ds)
        judgeDirac(d)
        d
    }
  }
}

@main def go() {
  import stuff._

  goDirac

  println(
    s"q1: ${Q1(Id(P(8L, 8, 0), P(3L, 3, 0), 1, BigInt(1L)))}"
    // Q1 (Id(P(4L, 4, 0), P(8L, 8, 0), 1, BigInt(1L)))
  )
}
