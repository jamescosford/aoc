// Scala 2.13.6

import scala.io.Source
import scala.annotation.tailrec

object stuff {

  /*


(0)
(6, 8)
(5, 7)
(4, 6)
(3, 5)
(2, 4)
(1, 3)
(0, 2)
(6, 1, 8)
(5, 0, 7, 8)
(4, 6, 6, 7)
(3, 5, 5, 6)
(2, 4, 4, 5)
(1, 3, 3, 4)
(0, 2, 2, 3)
(6, 1, 1, 2, 8)
(6, 0, 0, 1, 7)



n' = f(s, t, n)
 - s is the start age
 - t is the total time



n' =

   */

  def succ(i: Int): List[Int] =
    i match {
      case 0 => List(6, 8)
      case i => List(i - 1)
    }

  def suc(f: List[Int]) =
    f.flatMap(succ)

  def tSuc(f: List[Int], t: Int, res: List[Int]): List[Int] = t match {
    case -1 => res
    case _ =>
      val sf = suc(f) // compute successor list
      tSuc(
        sf,
        t - 1,
        res :+ (res.lastOption
          .getOrElse(1) + (sf.length - f.length)) // res is the generational total
      )
  }

}

@main
def go() {

  import stuff._
  // {
  //   val simT     = 18 // required simulation window
  //   val simS     = 8 // starting age of simfish
  //   val simTotal = simT + simS
  //   val data     = List(3, 4, 3, 1, 2)
  //   val lens     = stuff.tSuc(List(8), simTotal, List.empty).toIndexedSeq

  //   println()
  //   val tots = data.map(startAge => lens(simTotal - startAge - 1))
  //   println(tots)
  //   println(tots.sum)
  // }

  // {
  //   val simT     = 80
  //   val simS     = 8
  //   val simTotal = simT + simS

  //   val data = Source
  //     .fromFile("2021/day_006_input.txt")
  //     .getLines()
  //     .next()
  //     .split(',')
  //     .map(_.toInt)

  //   val lens = stuff.tSuc(List(8), simTotal, List.empty).toIndexedSeq
  //   val tots = data.map(startAge => lens(simTotal - startAge - 1))
  //   println(tots)
  //   println(tots.sum)
  // }

  {
    // val simT     = 256
    // val simS     = 8
    // val simTotal = simT + simS

    // val data = Source
    //   .fromFile("2021/day_006_input.txt")
    //   .getLines()
    //   .next()
    //   .split(',')
    //   .map(_.toInt)

    // val lens = stuff.tSuc(List(8), simTotal, List.empty).toIndexedSeq
    // val tots = data.map(startAge => lens(simTotal - startAge - 1))
    // println(tots)
    // println(tots.sum)

    val climbing = for {
      v <- Stream.from(2)
      s <- Stream.continually(v).take(7)
    } yield s

    val simT   = 80
    val newAtT = Stream.continually(1).take(9).concat(climbing).take(simT + 1).toIndexedSeq
    // newAtT.zipWithIndex.foreach(println)

    def forOne(age: Int, total: Int, time: Int, newAtT: IndexedSeq[Int]): Int =
      if (time <= 0)
        total
      else
        age match {
          case 0 =>
            // println(s"age:$age, time: $time, total: $total")
            forOne(6, total + newAtT(time - 1), time - 1, newAtT)
          case _ =>
            // println(s"age:$age, time: $time, total: $total")
            forOne(age - 1, total, time - 1, newAtT)
        }

    // val t3 = forOne(3, 1, 18, newAtT); println(t3)
    // val t4 = forOne(4, 1, 18, newAtT); println(t4)
    // val t2 = forOne(2, 1, 18, newAtT); println(t2)
    // val t1 = forOne(1, 1, 18, newAtT); println(t1)

    val data   = List(8)
    val result = data.map(v => forOne(v, 1, simT, newAtT)).sum
    println(result)

  }

  {

    val data = List(8)
    val lens = stuff.tSuc(List(8), 256, List.empty).toIndexedSeq
    println(lens)

    // println()
    // val tots = data.map(startAge => lens(simTotal - startAge - 1))
    // println(tots)
    // println(tots.sum)
  }

  // val lens = stuff.tSuc(List(8), 88, List.empty).toIndexedSeq

  // println(data.map(a => lens((18 + 8 - 1) - a)))

  // println(lens)
}

/*

java -Xmx24G -jar $(which amm) 2021/day_006.sc

epoch = 18

DP: (TTE, A) -> N

(0, _) -> 1
(1, 0) -> 2
(2, 0) -> 2
(3, 0) -> 2
(4, 0) -> 2
(5, 0) -> 2
(6, 0) -> 2
(7, 0) -> 2
(8, 0) -> 3


E 8 ->   0
E 7 -> 0 6
E 6 -> 6 5
E 5 -> 5 4
E 4 -> 4 3
E 3 -> 3 2
E 2 -> 2 1
E 1 -> 1 0
E 0 -> 0 6



 */
