// Scala 2.13.6

import scala.io.Source
import scala.annotation.tailrec

object stuff {

  type BingoNumbers = IndexedSeq[Int]

  case class Five[A](
    _1: A,
    _2: A,
    _3: A,
    _4: A,
    _5: A
  )

  object Five {
    def map[A, B](a: Five[A], f: A => B): Five[B] =
      Five[B](
        _1 = f(a._1),
        _2 = f(a._2),
        _3 = f(a._3),
        _4 = f(a._4),
        _5 = f(a._5)
      )
    def of[A](a: => A): Five[A] =
      Five[A](
        _1 = a,
        _2 = a,
        _3 = a,
        _4 = a,
        _5 = a
      )
    def toList[A](a: Five[A]): List[A] = List(a._1, a._2, a._3, a._4, a._5)

  }

  def numbersFromString(s: String): IndexedSeq[Int] =
    s.split(',').map[Int](_.toInt)

  type BingoRow = Five[(Int, Boolean)]
  def bingoRowFromString(s: String): BingoRow = {
    val row = s.trim.split("\\s+").map(_.toInt).take(5)
    Five(
      row(0) -> false,
      row(1) -> false,
      row(2) -> false,
      row(3) -> false,
      row(4) -> false
    )
  }

  type BingoBoard = Five[BingoRow]
  def bingoBoardFromStrings(i: List[String]): BingoBoard =
    i match {
      case _1 :: _2 :: _3 :: _4 :: _5 :: Nil =>
        Five.map[String, BingoRow](Five(_1, _2, _3, _4, _5), bingoRowFromString)
      case _ => ???
    }

  def loadBingoBoards(i: List[String], bbs: List[BingoBoard] = List.empty): List[BingoBoard] =
    i match {
      case Nil => bbs
      case _ =>
        val (forBoard, rem) = i.splitAt(5)
        loadBingoBoards(if (rem.isEmpty) Nil else rem.tail, bbs :+ bingoBoardFromStrings(forBoard))
    }

  case class TrackedBB(
    bb: BingoBoard,
    rowTally: Five[Int] = Five.of(0),
    colTally: Five[Int] = Five.of(0)
  )

  def stepRow(r: BingoRow, num: Int): BingoRow =
    r.copy(
      _1 = r._1._1 -> (r._1._2 || r._1._1 == num),
      _2 = r._2._1 -> (r._2._2 || r._2._1 == num),
      _3 = r._3._1 -> (r._3._2 || r._3._1 == num),
      _4 = r._4._1 -> (r._4._2 || r._4._1 == num),
      _5 = r._5._1 -> (r._5._2 || r._5._1 == num)
    )

  def transpose(bb: BingoBoard): BingoBoard =
    bb match {
      case Five(_1, _2, _3, _4, _5) =>
        Five(
          Five(_1._1, _2._1, _3._1, _4._1, _5._1),
          Five(_1._2, _2._2, _3._2, _4._2, _5._2),
          Five(_1._3, _2._3, _3._3, _4._3, _5._3),
          Five(_1._4, _2._4, _3._4, _4._4, _5._4),
          Five(_1._5, _2._5, _3._5, _4._5, _5._5)
        )
    }

  def stepBB(_bb: TrackedBB, num: Int): TrackedBB = {
    val TrackedBB(bb, rt, ct) = _bb
    val updated               = Five.map[BingoRow, BingoRow](bb, stepRow(_, num))
    val newRt = Five.map[BingoRow, Int](updated, br => {
      Five.toList(br).filter(_._2).length
    })
    val newCt = Five.map[BingoRow, Int](transpose(updated), br => {
      Five.toList(br).filter(_._2).length
    })
    TrackedBB(updated, newRt, newCt)
  }

  def tallyRow(r: BingoRow): Int =
    Five.toList(r).filter(_._2 != true).map(_._1).sum

  def complete(_bb: TrackedBB): Option[Int] = {
    val complete = Five.toList(_bb.rowTally).exists(_ == 5) ||
      Five.toList(_bb.colTally).exists(_ == 5)
    if (!complete)
      return None
    Some(Five.toList(Five.map[BingoRow, Int](_bb.bb, tallyRow)).sum)
  }

}

@main
def go() {

  import stuff._

  val data = Source
    .fromFile("2021/day_004_input.txt")
    .getLines()
    .toList

  val bns   = stuff.numbersFromString(data.head)
  val space = data.tail
  val bbs   = stuff.loadBingoBoards(space.tail).map(stuff.TrackedBB(_))

  val res = bns.foldLeft[Either[List[TrackedBB], Int]](Left(bbs)) {
    case (Left(tbbs), num) =>
      val _tbbs = tbbs.map(stepBB(_, num))
      val cplt  = _tbbs.map(complete).filter(_.isDefined).map(_.get).headOption
      cplt match {
        case Some(value) => Right(value * num)
        case None        => Left(_tbbs)
      }
    case (Right(res), _) => Right(res)
  }

  val res2 = bns.foldLeft[(List[TrackedBB], Option[Int])](bbs -> None) {
    case ((tbbs, o), num) =>
      val _tbbs  = tbbs.map(stepBB(_, num))
      val cplt   = _tbbs.map(complete)
      val __tbbs = _tbbs.zip(cplt).filter(_._2.isEmpty).map(_._1)
      val latest = cplt.filter(_.isDefined).headOption.flatten.map(_ * num)
      (__tbbs, if (latest.isDefined) latest else o)
  }
  println(res2)
}
