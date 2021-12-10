// Scala 2.13.6

import scala.io.Source
import scala.annotation.tailrec

@main
def go() {

  val data = Source
    .fromFile("2021/day_006_input.txt")
    .getLines()
    .next()
    .split(',')
    .map(_.toInt)

  val init = data.foldLeft[IndexedSeq[Long]](IndexedSeq.fill(9)(0L)) {
    case (acc, age) => acc.updated(age, acc(age) + 1L)
  }

  def suc(state: IndexedSeq[Long], t: Int): IndexedSeq[Long] = {
    if (t == 0)
      state
    else
      state match {
        case Seq(_0, _1, _2, _3, _4, _5, _6, _7, _8) =>
          suc(IndexedSeq(_1, _2, _3, _4, _5, _6, _7 + _0, _8, _0), t - 1)
        case _ => ???
      }
  }

  println(suc(init, 256).sum)

}
