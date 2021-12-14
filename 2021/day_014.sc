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

  def tally(s: String): Map[Char, Long] =
    s.foldLeft(Map.empty[Char, Long]) {
      case (acc, c) => acc + (c -> (acc.get(c).getOrElse(0L) + 1L))
    }

  def forT(t: Int, template: String, rules: Map[String, String]): Long = { // ORIGINAL VERSION EATS MEMORY
    val charTally = tally((0 until t).foldLeft(template) {
      case (s, _) =>
        s.sliding(2)
          .map[String] { pair =>
            rules.get(pair).map(c => s"${pair(0)}$c").getOrElse(s"${pair(0)}")
          }
          .mkString + s.last
    }).toList.map(_._2).sorted
    charTally.last - charTally.head
  }

  // CONSTANT MEM IMPLEMENTATION STILL TAKES FOREVER
  def rec(
    template: String,
    rules: Map[(Char, Char), (Char, Char, Char)],
    t: Int
  ): Map[Char, Long] = {

    val _tally = scala.collection.mutable.Map.empty[Char, Long]

    def inc(c: Char, tally: Map[Char, Long]): Map[Char, Long] =
      tally + (c -> (tally.get(c).getOrElse(0L) + 1L))

    def left(_1: Char, _2: Char, t: (Int, Int), tally: Map[Char, Long]): (Char, Map[Char, Long]) =
      if (t._1 == t._2)
        _2 -> tally
      else {
        val rule  = rules(_1 -> _2)
        val tnext = t._1 -> (t._2 + 1)
        val (c, leftTally) = left(
          rule._1,
          rule._2,
          tnext,
          inc(rule._2, tally)
        )
        val (d, rightTally) = left(
          c,
          _2,
          tnext,
          leftTally
        )
        d -> rightTally
      }
    inc(template.last, template.toCharArray().sliding(2).foldLeft(Map.empty[Char, Long]) {
      case (acc, cs) =>
        left(cs(0), cs(1), t -> 0, inc(cs(0), acc))._2
    })
  }

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
