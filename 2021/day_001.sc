// Scala 2.13.6

import scala.io.Source

@main
def go() {
  // Source.fromFile()
  
  println(Source
    .fromFile("2021/day_001_input.txt")
    .getLines()
    .map(_.toInt)
    .sliding(2)
    .foldLeft[Int](0) {
      case (tally, Seq(a, b)) => tally + (if (b > a) 1 else 0)
    }
  )
  
  println(Source
    .fromFile("2021/day_001_input.txt")
    .getLines()
    .map(_.toInt)
    .sliding(3)
    .map { _.sum }
    .sliding(2)
    .foldLeft[Int](0) {
      case (tally, Seq(a, b)) => tally + (if (b > a) 1 else 0)
    }
  )
}