// Scala 2.13.6

import scala.annotation.tailrec
import scala.io.Source

object stuff {

  val pushToPop = Map(
    '{' -> '}',
    '(' -> ')',
    '<' -> '>',
    '[' -> ']'
  )

  val popToPush = pushToPop.map(x => x.swap)

  val scores = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  val q2scores = Map(
    ')' -> 1,
    ']' -> 2,
    '}' -> 3,
    '>' -> 4
  )

  def handleChar(stk: List[Char], c: Char): Either[Char, List[Char]] = {
    if (pushToPop.contains(c))
      return Right(c +: stk)
    if (stk.isEmpty || popToPush(c) != stk.head)
      return Left(c)
    return Right(stk.tail)
  }

  @tailrec
  def firstInvalid(stk: List[Char], str: String): Either[Char, List[Char]] = {
    if (str.isEmpty)
      return Right(stk)
    handleChar(stk, str.head) match {
      case Left(c)    => Left(c)
      case Right(stk) => firstInvalid(stk, str.tail)
    }
  }

}

@main
def go() {

  import stuff._

  val data = Source
    .fromFile("2021/day_010_input.txt")
    .getLines()
    .toList

  val q1Res = data.flatMap(l => firstInvalid(List.empty, l).swap.toOption).map(scores).sum
  println(q1Res)

  val q2Res = data
    .map(s => s -> firstInvalid(List.empty, s).toOption)
    .flatMap {
      case (s, ostk) => ostk.map(stk => s -> stk)
    }
    .map {
      case (s, stk) =>
        stk.map(pushToPop).mkString.foldLeft(0L) {
          case (acc, c) => 5 * acc + q2scores(c)
        }
    }
    .sorted
  println(q2Res(q2Res.length / 2))
}
