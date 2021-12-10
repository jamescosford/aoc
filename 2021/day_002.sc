// Scala 2.13.6

import scala.io.Source

@main
def go() {

  sealed abstract class Command(delta: Int)
  object Command {
    case class Forward(delta: Int) extends Command(delta)
    case class Down(delta: Int)    extends Command(delta)
    case class Up(delta: Int)      extends Command(delta)

    val forward = "forward ([0-9]+)".r
    val down    = "down ([0-9]+)".r
    val up      = "up ([0-9]+)".r

    def lineToCommand(s: String): Command = s match {
      case forward(delta) => Forward(delta.toInt)
      case down(delta)    => Down(delta.toInt)
      case up(delta)      => Up(delta.toInt)
    }
  }

  println(
    Option(
      Source
        .fromFile("2021/day_002_input.txt")
        .getLines()
        .map(Command.lineToCommand)
        .foldLeft[(Int, Int)](0 -> 0) {
          case ((h, v), cmd) =>
            cmd match {
              case Command.Forward(delta) => (h + delta) -> v
              case Command.Down(delta)    => h           -> (v + delta)
              case Command.Up(delta)      => h           -> (v - delta)
            }
        }
    ).map {
      case (h, v) => h * v
    }.get
  )

  println(
    Option(
      Source
        .fromFile("2021/day_002_input.txt")
        .getLines()
        .map(Command.lineToCommand)
        .foldLeft[(Int, Int, Int)]((0, 0, 0)) {
          case ((aim, h, v), cmd) =>
            cmd match {
              case Command.Down(delta)    => (aim + delta, h, v)
              case Command.Up(delta)      => (aim - delta, h, v)
              case Command.Forward(delta) => (aim, h + delta, v + aim * delta)
            }
        }
    ).map {
      case (_, h, v) => h * v
    }.get
  )

}
