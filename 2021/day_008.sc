// Scala 2.13.6

import scala.io.Source

object stuff {

  val re = "([a-z ]+) \\| ([a-z ]+)".r

  type Seg = Set[Char]

  type Result = Map[Int, Seg]

  type Input = Array[Seg]

  type Output = Array[Seg]

  type Line = (Input, Output)

  def toSegs(s: String): Array[Seg] =
    s.split(" ").map(_.toSet).toArray

  def toLine(s: String): Line = s match {
    case re(i, o) =>
      toSegs(i) -> toSegs(o)
  }

  def toVal(in: Set[Char], _1: Set[Char], _4: Set[Char], _7: Set[Char]): (Set[Char], Int) = {
    (in.size, (in & _1).size, (in & _4).size, (in & _7).size) match {
      case (2, _, _, _) => in -> 1
      case (5, 1, 2, 2) => in -> 2
      case (5, 2, 3, 3) => in -> 3
      case (4, _, _, _) => in -> 4
      case (5, 1, 3, 2) => in -> 5
      case (6, 1, 3, 2) => in -> 6
      case (3, _, _, _) => in -> 7
      case (7, _, _, _) => in -> 8
      case (6, 2, 4, 3) => in -> 9
      case (6, 2, 3, 3) => in -> 0
    }
  }

  def toKnownVals(i: Array[Seg]): (Set[Char], Set[Char], Set[Char]) = (
    i.filter(_.size == 2).head,
    i.filter(_.size == 4).head,
    i.filter(_.size == 3).head
  )

}

@main
def go() {

  import stuff._

  val data = Source
    .fromFile("2021/day_008_input.txt")
    .getLines()
    .map(toLine)
    .toList

  val l1478 = Set(2, 4, 3, 7)

  val q1 = data.flatMap(_._2.toList.map(_.size)).filter(x => l1478.contains(x)).length

  val q2 = data.map {
    case (input, output) =>
      val known      = toKnownVals(input)
      val translator = input.map(toVal(_, known._1, known._2, known._3)).toMap
      output.map(translator).map(_.toInt).mkString.toInt
  }.sum
  println(q2)
}

/*

  f = 8 diff ()
  g =


  4 diff 1 = (b, d)
  9 diff 8 = (e)
  6 diff 8 = (c)
  6 diff 5 = (e)
  8 diff 0 = (d)
  8 diff 2 = (b, f)
  3 diff 2 = (e, f)

  We know 1, 4, 7, 8

  a = 1 diff 7
  b = (4 diff 1) int (8 diff 2_)
  c
  d
  e =
  f
  g


  /*
  ___________________________
  > 1 = (    c     f  ) x
  > 2 = (a   c d e   g)   : l = 5 (i 1 = 1, i 4 = 2, i 7 = 2)
  > 3 = (a   c d   f g)   : l = 5 (i 1 = 2, i 4 = 3, i 7 = 3)
  > 4 = (  b c d   f  ) x
  > 5 = (a b   d   f g)   : l = 5 (i 1 = 1, i 4 = 3, i 7 = 2)
  > 6 = (a b   d e f g)   : l = 6 (i 1 = 1, i 4 = 3, i 7 = 2)
  > 7 = (a   c     f  ) x
  > 8 = (a b c d e f g) x
  > 9 = (a b c d   f g)   : l = 6 (i 1 = 2, i 4 = 4, i 7 = 3)
  > 0 = (a b c   e f g)   : l = 6 (i 1 = 2, i 4 = 3, i 7 = 3)
 */


 */
