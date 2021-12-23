// Scala 2.13.6

import scala.io.Source

/*

Each pixel of the output image is determined by looking at a 3x3 square of pixels
centred on the corresponding input image pixel. These nine input pixels are comb-
ined into a single binary number that is used as an index in the image ehnahceme-
nt algorithm string.

 */

object stuff {
  type XY  = (Int, Int)
  type Img = (XY, Set[XY])

  def doubleNeighbs(xy: XY, bounds: XY): IndexedSeq[XY] =
    (for {
      dx <- (-2 to 2)
      dy <- (-2 to 2)
    } yield (xy._1 + dx, xy._2 + dy))

  def neighbs(xy: XY): IndexedSeq[XY] =
    IndexedSeq(
      (-1, -1),
      (0, -1),
      (1, -1),
      (-1, 0),
      (0, 0),
      (1, 0),
      (-1, 1),
      (0, 1),
      (1, 1)
    ).map {
      case (dx, dy) => (xy._1 + dx, xy._2 + dy)
    }

  def sample(grid: IndexedSeq[String], c: XY): String =
    neighbs(c).map {
      case (x, y) if x < 0 || y < 0 || x >= grid(0).length || y >= grid.length => '0'
      case (x, y) =>
        val p = grid(y)(x)
        if (p == '.') '0' else '1'
    }.mkString

  def sample(img: Img, c: XY): String =
    neighbs(c).map {
      case (x, y) if img._2.contains(x -> y) => '1'
      case _ => '0'
    }.mkString

  def emptyS(l: Int): String = Stream.continually('.').take(l).mkString

  def toBin(s: String): Int = Integer.parseInt(s, 2)

  def _grow(grid: IndexedSeq[String]): IndexedSeq[String] = {
    val ly    = grid.length
    val lx    = grid(0).length
    val empty = emptyS(lx + 4)
    val emp   = emptyS(2)
    IndexedSeq(empty, empty) ++ grid.map(r => emp + r + emp) ++ IndexedSeq(empty, empty)
  }

  def grow(img: Img, amt: Int): Img =
    (img._1._1 + 2 * amt, img._1._2 + 2 * amt) ->
      img._2.map {
        case (x, y) => (x + amt, y + amt)
      }

  def shrink(img: Img, amt: Int): Img = {
    val bounds = (img._1._1 - 2 * amt, img._1._2 - 2 * amt)
    val i = img._2
      .map {
        case (x, y) => (x - amt, y - amt)
      }
      .filter {
        case (x, y) => x >= 0 && x < bounds._1 && y >= 0 && y < bounds._2
      }
    bounds -> i
  }

  def _show(grid: IndexedSeq[String]): String =
    grid.mkString(System.lineSeparator())

  def show(img: Img): String =
    (0 until img._1._2)
      .map { y =>
        (0 until img._1._1).map { x =>
          Option.when(img._2.contains(x -> y))('#').getOrElse('.')
        }.mkString
      }
      .mkString(System.lineSeparator)

  def toImg(grid: IndexedSeq[String]): Img =
    (grid(0).length, grid.length) ->
      grid.zipWithIndex.flatMap {
        case (row, y) =>
          row.toIndexedSeq.zipWithIndex.flatMap {
            case (c, x) => Option.when(c == '#')((x -> y))
          }
      }.toSet

  def applyAlg(img: Img, alg: String): Img =
    img._1 ->
      img._2.foldLeft(Set.empty[XY]) {
        case (acc, xy) =>
          acc ++ doubleNeighbs(xy, img._1)
            .filterNot(acc.contains)
            .flatMap { xy =>
              Option.when(alg(toBin(sample(img, xy))) == '#')(xy)
            }
            .toSet
      }

  def applyAlg_(img: Img, alg: String): Img =
    img._1 -> {
      (for {
        x <- (1 until img._1._1 - 1)
        y <- (1 until img._1._2 - 1)
      } yield (x -> y)).flatMap { xy =>
        if (xy._2 == img._1._2 - 1) {
          val bin = sample(img, xy)
          val ind = toBin(bin)
          println(bin)
          println(ind)
          println(alg(ind))

        }
        Option.when(alg(toBin(sample(img, xy))) == '#')(xy)

      }.toSet
    }

  def compute(img: Img, alg: String, times: Int): Img =
    (0 until times).foldLeft(img) {
      case (i, t) =>
        val nimg = shrink(applyAlg_(grow(i, t), alg), t + 1)

        // val nimg = applyAlg_(i, alg)
        // println("==========================")
        // println(show(nimg))
        nimg
    }
}

@main def go() {
  import stuff._

  val grid = Source
    .fromFile("2021/day_020_input.txt")
    // .fromFile("2021/day_020_test_input.txt")
    // .fromFile("2021/day_020_test_input_2.txt")
    .getLines()
    .toIndexedSeq

  val (_alg, rem) = grid.splitAt(1)
  val alg         = _alg.head

  val img  = rem.tail
  val img2 = _grow(img)

  // println(alg)
  // println(_show(img))
  // println(_show(img2))

  // println()

  // println(show(toImg(img)))
  // println(show(grow(toImg(img))))

  // val img_ = toImg(img)
  // println(img_)
  // val res = compute(img_, alg, 2)
  // println(res)

  val imgg = toImg(img)
  println(imgg._1)
  val res = compute(grow(toImg(img), 150), alg, 50)
  val rss = res //shrink(res, 1)
  println(show(rss))
  println(rss._2.size)

  // val t = grow(toImg(img), 5)
  // println(show(t))
  // println(show(shrink(t, 2)))

  // println(toBin(sample(toImg(img), 2 -> 2)))

}

// fails
// 5834
// 6581
// 6153
