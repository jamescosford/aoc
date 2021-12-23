// Scala 2.13.6
import scala.io.Source

object stuff {}

@main
def go() {

  import stuff._

  val target = (
    (179 until 201),
    (-109 until -63)
  )

  // `Σt_0-t` = n/2 (start + end)

  // given y is below 0, and we want a positive parabola, we know that x will reach max displacement
  // SO, maxx
  // x(t) = x0 +

  def maxx_vx(vx: Int): Int = vx * vx / 2

  def v_t(x0: Int, v0: Int, t: Int): Int = (0 until t).foldLeft(x0) {
    case (x, t) => (x + math.max(v0 - t, 0))
  }

  def xs_t(x0: Int, v0: Int, t0: Int): List[(Int, Int)] =
    Stream
      .from(t0)
      .takeWhile(_ < 100)
      .map(t => t -> v_t(x0, v0, t))
      .map { x =>
        println(x); x
      }
      .takeWhile(_._2 < target._1.max)
      .toList

  // def y_t(x0: Int, v0: Int, t: Int): Int = x0 + (t / 2) * ((v0 - (t - 1)) + v0)

  def y_t(x0: Int, v0: Int, t: Int): Int = (0 until t).foldLeft(x0) {
    case (x, t) => (x + v0 - t)
  }

  def ys_t(x0: Int, v0: Int, t0: Int): List[(Int, Int)] = {
    val ys = Stream
      .from(t0)
      .map(t => t -> y_t(x0, v0, t))
      .takeWhile(_._2 > target._2.min)
      .toList
    // println(ys)
    ys
  }

  // def p_t(x0: (Int, Int), v0: (Int, Int))(t: Int): (Int, Int)

  def p_t(x0: (Int, Int), v0: (Int, Int))(t: Int): (Int, Int) = {
    (
      x0._1 + ((t + 1) / 2) * (math.max(0, v0._1 - (t - 1)) + v0._1),
      // x0._1 + (t / 2) * (v0._1 - t),
      x0._2 + (t / 2) * ((v0._2 - (t - 1)) + v0._2)
    )
  }

  println(p_t(0 -> 0, 1 -> 1)(1))
  println(p_t(0 -> 0, 4 -> 1)(2))
  println(p_t(0 -> 0, 4 -> 1)(3))
  println(p_t(0 -> 0, 4 -> 1)(4))

  val vx = Stream
    .from(0)
    .map(v => v -> maxx_vx(v))
    .dropWhile(p => !target._1.contains(p._2))
    .takeWhile(p => target._1.contains(p._2))
    .toList
    .sortBy(_._1)
    .last

  println(vx)

  val x = Stream
    .from(0)
    .map(v0 => v0 -> xs_t(0, v0, 0))
    .dropWhile(x => !target._1.contains(x._2))
    .foreach(println)

  val y = Stream
    .from(-10)
    .map(v0 => v0 -> ys_t(0, v0, 0).map(_._2))
    .take(200)
    .filter(_._2.exists(target._2.contains))
    .sortBy(_._2.max)
    .map(_._2.max)
    .last

  println(y)
}
