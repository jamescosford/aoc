// Scala 2.13.6
import scala.io.Source
import scala.annotation.tailrec

object stuff {

  def lastLB(stk: IndexedSeq[Tok]): `[` =
    stk
      .collect[`[`] {
        case x: `[` => x
      }
      .reverse
      .headOption
      .getOrElse(`[`(0))

  sealed trait Tok
  case class `[`(d: Int) extends Tok
  case object `]`        extends Tok
  case object `,`        extends Tok
  case class D(v: Int)   extends Tok

  val rx_lb = "^\\[(.*)$".r
  val rx_d  = "^(\\d+)(.*)$".r
  val rx_c  = "^,(.*)$".r
  val rx_rb = "^](.*)$".r
  def parse(s: String, toks: IndexedSeq[Tok] = IndexedSeq.empty): IndexedSeq[Tok] =
    s match {
      case rx_lb(rem) =>
        parse(rem, toks :+ `[`(lastLB(toks).d + 1))
      case rx_d(d, rem) =>
        parse(rem, toks :+ D(d.toInt))
      case rx_c(rem) =>
        parse(rem, toks :+ `,`)
      case rx_rb(rem) =>
        parse(rem, toks :+ `]`)
      case "" =>
        toks
    }

  def show(ts: Seq[Tok]): String = ts.foldLeft("") {
    case (acc, t) =>
      t match {
        case D(v)   => acc + v.toString()
        case `[`(d) => acc + '['
        case `]`    => acc + ']'
        case `,`    => acc + ','
      }
  }

  def parseTest = {
    show(parse("[[[[[9,8],1],2],3],4]"))
  }

  @tailrec
  def findSplit(ts: IndexedSeq[(Tok, Int)]): Option[Int] =
    ts.headOption match {
      case Some((t, i)) =>
        t match {
          case D(v) if v >= 10 => Some(i)
          case _               => findSplit(ts.tail)
        }
      case None => None
    }

  def explode(ts: IndexedSeq[Tok]): Option[IndexedSeq[Tok]] =
    findExplode(ts.zipWithIndex).map {
      case (i, l, r) => doExplode(ts, i, l, r)
    }

  @tailrec
  def findLeaves(ts: IndexedSeq[(Tok, Int)]): Option[(Int, D, D)] =
    if (ts.length < 5)
      None
    else
      ts match {
        case IndexedSeq((`[`(_), i), (l @ D(a), _), (`,`, _), (r @ D(b), _), (`]`, _), _*) =>
          Some((i, l, r))
        case IndexedSeq((`[`(_), _), _*) => findLeaves(ts.tail)
        case IndexedSeq((`]`, _), _*)    => findLeaves(ts.tail)
        case _                           => findLeaves(ts.tail)
      }

  @tailrec
  def findExplode(ts: IndexedSeq[(Tok, Int)], d: Int = 0): Option[(Int, D, D)] =
    if (ts.length < 5)
      None
    else
      ts match {
        case IndexedSeq((`[`(_), i), (l @ D(a), _), (`,`, _), (r @ D(b), _), (`]`, _), _*)
            if d > 3 =>
          Some((i, l, r))
        case IndexedSeq((`[`(_), _), _*) => findExplode(ts.tail, d + 1)
        case IndexedSeq((`]`, _), _*)    => findExplode(ts.tail, d - 1)
        case _                           => findExplode(ts.tail, d)
      }

  def explodeTest = {
    def run(in: String, exp: String) = {
      val ts  = parse(in)
      val res = explode(ts).map(show).get
      assert(res == exp)
    }
    run("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]")
    run("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]")
    run("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
    run("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
  }

  def splitTest = {
    def run(in: String, exp: String) = {
      val ts  = parse(in)
      val res = split(ts).map(show).get
      assert(res == exp)
    }
    run("[[[[10,8],1],2],3]", "[[[[[5,5],8],1],2],3]")
  }

  @tailrec
  def mapFirst[A](f: A => Option[A], as: Seq[A], seen: Seq[A] = Seq.empty): Seq[A] =
    as.headOption match {
      case None => seen
      case Some(a) =>
        f(a) match {
          case Some(a_) => (seen :+ a_) ++ as.tail
          case None     => mapFirst(f, as.tail, seen :+ a)
        }
    }

  def addToD(v: Int)(t: Tok): Option[Tok] =
    t match {
      case D(a) => Some(D(a + v))
      case _    => None
    }

  def doExplode(ts: IndexedSeq[Tok], i: Int, l: D, r: D): IndexedSeq[Tok] = {
    val left  = mapFirst[Tok](addToD(l.v), ts.splitAt(i)._1.reverse).reverse
    val right = mapFirst[Tok](addToD(r.v), ts.splitAt(i + 5)._2)
    val res   = (left :+ D(0)) ++ right
    res.toIndexedSeq
  }

  def doMagnitude(ts: IndexedSeq[Tok], i: Int, l: D, r: D): (BigInt, IndexedSeq[Tok]) = {
    val left  = ts.splitAt(i)._1
    val right = ts.splitAt(i + 5)._2
    val res   = (left :+ D(3 * l.v + 2 * r.v)) ++ right
    BigInt(3 * l.v + 2 * r.v) -> res.toIndexedSeq
  }

  def doSplit(ts: IndexedSeq[Tok], i: Int): IndexedSeq[Tok] = {
    val left  = ts.splitAt(i)._1
    val right = ts.splitAt(i + 1)._2
    val t = ts(i) match {
      case D(v) =>
        IndexedSeq[Tok](
          `[`(lastLB(left).d + 1),
          D(math.floor(v.toDouble / 2d).toInt),
          `,`,
          D(math.ceil(v.toDouble / 2d).toInt),
          `]`
        )
      case _ => IndexedSeq.empty[Tok]
    }
    left ++ t ++ right
  }

  def split(ts: IndexedSeq[Tok]): Option[IndexedSeq[Tok]] =
    findSplit(ts.zipWithIndex).map { i =>
      doSplit(ts, i)
    }

  def or[A](a: => Option[A], b: => Option[A]): Option[A] =
    a match {
      case s @ Some(_) => s
      case _           => b
    }

  def doAdd(a: IndexedSeq[Tok], b: IndexedSeq[Tok]): IndexedSeq[Tok] =
    IndexedSeq(
      `[`(1)
    ) ++ a ++ IndexedSeq(`,`) ++ b ++ IndexedSeq(`]`)

  def add(a: IndexedSeq[Tok], b: IndexedSeq[Tok]): IndexedSeq[Tok] = {
    val toReduce = doAdd(a, b)
    LazyList
      .unfold[IndexedSeq[Tok], IndexedSeq[Tok]](toReduce) { bt =>
        // println(show(bt))
        or(
          explode(bt),
          split(bt)
        ) match {
          case Some(bt_) => Some(bt_ -> bt_)
          case _         => None
        }
      }
      .toList
      .lastOption
      .getOrElse(toReduce)
  }

  def magnitude(ts: IndexedSeq[Tok]): BigInt =
    LazyList
      .unfold[BigInt, (BigInt, IndexedSeq[Tok])](BigInt(0) -> ts) {
        case (d, ts) =>
          findLeaves(ts.zipWithIndex).map {
            case (i, l, r) =>
              val a @ (d_, _) = doMagnitude(ts, i, l, r)
              d_ -> a
          }
      }
      .last

  def addAll(ss: Iterator[String]): IndexedSeq[Tok] = {
    val sns  = LazyList.from(ss).map(parse(_))
    val init = (sns.head, sns.tail)

    LazyList
      .unfold[IndexedSeq[Tok], (IndexedSeq[Tok], LazyList[IndexedSeq[Tok]])](init) {
        case (sn, rem) =>
          rem.headOption match {
            case None => None
            case Some(value) =>
              val v = add(sn, value)
              Some(v -> (v -> rem.tail))
          }
      }
      .last
  }

}
@main
def go() {
  import stuff._
  parseTest
  explodeTest
  splitTest

  val lines = Source
    .fromFile("2021/day_018_input.txt")
    .getLines()
    .toList

  val added = addAll(lines.iterator)
  println(show(added))
  val magn = magnitude(added)
  println(magn)

  val parsed = lines.map(parse(_))

  val part2 = (for {
    a <- LazyList.from(parsed)
    b <- LazyList.from(parsed)
  } yield (a, b))
    .filter {
      case (a, b) => a != b
    }
    .foldLeft[BigInt](BigInt(0)) {
      case (acc, (a, b)) =>
        val res = magnitude(add(a, b))
        if (res > acc)
          res
        else acc
    }
  println(part2)
}
