// Scala 2.13.6

import scala.util.matching.Regex
import scala.annotation.tailrec
import scala.io.Source

object stuff {

  val templateRx = "([A-Z]+)".r
  val ruleRx     = "([A-Z]+) -> ([A-Z])".r
  def parse(ss: List[String]): (String, Map[String, String]) =
    (ss.head match {
      case templateRx(t) => t
    }, ss.tail.tail.map { s =>
      s match {
        case ruleRx(a, b) => a -> b
      }
    }.toMap)

  def inc[A](s: A, tally: Map[A, Long]): Map[A, Long] =
    add(s, tally, 1L)
  def add[A](s: A, tally: Map[A, Long], n: Long): Map[A, Long] =
    tally + (s -> (tally.get(s).getOrElse(0L) + n))

  // Once I realised this is the EXACT SAME QUESTION as day 6
  def compute(t: Int, template: String, rules: Map[String, String]): Long = {
    val pairMap = template.sliding(2).foldLeft(rules.toList.map(_._1 -> 0L).toMap) {
      case (acc, s) => inc(s, acc)
    }
    val charMap = template.foldLeft(Map.empty[Char, Long]) {
      case (acc, c) => inc(c, acc)
    }
    val res = (0 until t)
      .foldLeft(pairMap -> charMap) {
        case ((pm, cm), _) =>
          val update = pm.toList
            .map {
              case (pair, n) =>
                val rule = rules(pair)
                (
                  (rule(0), n),
                  List(
                    s"${pair(0)}$rule"   -> n,
                    s"${rule}${pair(1)}" -> n,
                    pair                 -> -n
                  )
                )
            }
          val _cm = update.map(_._1).foldLeft(cm) {
            case (acc, (k, v)) => add(k, acc, v)
          }
          val _pm = update.flatMap(_._2).foldLeft(pm) {
            case (acc, (k, v)) => add(k, acc, v)
          }
          _pm -> _cm
      }
      ._2
      .toList
      .map(_._2)
      .sorted

    res.last - res.head
  }

}

@main
def go() {

  import stuff._

  val (template, rules) = parse(
    Source
      .fromFile("2021/day_014_input.txt")
      .getLines()
      .toList
  )

  println(compute(10, template, rules))
  println(compute(40, template, rules))

}
