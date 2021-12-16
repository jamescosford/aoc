// Scala 2.13.6
import scala.io.Source

object stuff {

  sealed trait Operation
  object Operation {
    case object Sum         extends Operation
    case object Product     extends Operation
    case object Minimum     extends Operation
    case object Maximum     extends Operation
    case object GreaterThan extends Operation
    case object LessThan    extends Operation
    case object EqualTo     extends Operation

    val opMap = Map(
      0 -> Sum,
      1 -> Product,
      2 -> Minimum,
      3 -> Maximum,
      5 -> GreaterThan,
      6 -> LessThan,
      7 -> EqualTo
    )
  }

  sealed trait Packet {
    val version: Int
  }
  object Packet {
    case class Literal(version: Int, value: Long)                           extends Packet
    case class Operator(version: Int, op: Operation, packets: List[Packet]) extends Packet

  }

  val toBinMap = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111"
  )
  def toBinary(s: String): String =
    s.flatMap(toBinMap)

  def mapLeft[A, B, C](t: Tuple2[A, B], f: A => C): Tuple2[C, B] =
    f(t._1) -> t._2

  object parse {
    def bitsToInt(s: String): Int =
      Integer.parseInt(s, 2) // ints too big!
    def bitsToLong(s: String): Long =
      BigInt(s, 2).toLong

    def IntN(s: String, n: Int): (Int, String) =
      mapLeft(s.splitAt(n), bitsToInt)
    def Int3(s: String): (Int, String) =
      IntN(s, 3)
    def Int4(s: String): (Int, String) =
      IntN(s, 4)

    def LiteralVal(s: String): (Long, String) = {
      def rec(s: String): (String, String) =
        s.head match {
          case '1' =>
            val (numPart, rem) = s.tail.splitAt(4)
            val (np, lastRem)  = rec(rem)
            numPart + np -> lastRem
          case '0' => s.tail.splitAt(4)
        }
      mapLeft(rec(s), bitsToLong)
    }

    def _Packet(s: String): (Packet, String) = {
      val (v, r0) = Int3(s)
      val (t, r1) = Int3(r0)
      t match {
        case 4 =>
          mapLeft(LiteralVal(r1), lv => Packet.Literal(v, lv))
        case op =>
          mapLeft(SubPackets(r1), o => Packet.Operator(v, Operation.opMap(op), o))
      }
    }

    def SubPackets(s: String): (List[Packet], String) =
      s.head match {
        case '0' =>
          val (l, r) = mapLeft(s.tail.splitAt(15), bitsToInt)
          PacketsUntil(r, l)
        case '1' =>
          val (n, r) = mapLeft(s.tail.splitAt(11), bitsToInt)
          PacketsN(r, n)
        case _ => ???
      }

    def PacketsUntil(_s: String, _l: Int): (List[Packet], String) = {
      def rec(s: String, acc: List[Packet]): (List[Packet], String) =
        if (_s.length - s.length == _l)
          acc -> s
        else {
          val (p, r) = _Packet(s)
          rec(r, acc :+ p)

        }
      rec(_s, List.empty)
    }

    def PacketsN(s: String, n: Int): (List[Packet], String) = {
      def rec(s: String, acc: List[Packet]): (List[Packet], String) =
        if (acc.length == n)
          acc -> s
        else {
          val (p, r) = _Packet(s)
          rec(r, acc :+ p)
        }
      rec(s, List.empty)
    }
  }

  def sumVersions(p: Packet): Int = {
    def rec(acc: Int, p: Packet): Int =
      p match {
        case Packet.Literal(v, _) => acc + v
        case Packet.Operator(v, _, ps) =>
          acc + v + ps.foldLeft(0) {
            case (acc, p) => acc + sumVersions(p)
          }
      }
    rec(0, p)
  }

  def compute(p: Packet): Long =
    p match {
      case Packet.Literal(version, value) => value
      case Packet.Operator(version, op, packets) => {
        val ps = packets.map(compute)
        op match {
          case Operation.Sum     => ps.sum
          case Operation.Product => ps.product
          case Operation.Minimum => ps.min
          case Operation.Maximum => ps.max
          case Operation.GreaterThan =>
            if (ps(0) > ps(1)) 1L else 0L
          case Operation.LessThan =>
            val bits = packets.map(compute)
            if (ps(0) < ps(1)) 1L else 0L
          case Operation.EqualTo =>
            val bits = packets.map(compute)
            if (ps(0) == ps(1)) 1L else 0L
        }
      }
    }

  def versionSumFromHex(h: String): Int = {
    val bin    = toBinary(h)
    val packet = parse._Packet(bin)
    sumVersions(packet._1)
  }

  def resultFromHex(h: String): Long = {
    val bin    = toBinary(h)
    val packet = parse._Packet(bin)
    compute(packet._1)
  }

}

@main
def go() {

  import stuff._

  val hex = Source
    .fromFile("2021/day_016_input.txt")
    .getLines()
    .toList
    .head

  assert(toBinary("38006F45291200") == "00111000000000000110111101000101001010010001001000000000")
  assert(toBinary("EE00D40C823060") == "11101110000000001101010000001100100000100011000001100000")
  assert(
    parse
      .Int3("110100101111111000101000") == (6, "100101111111000101000")
  )
  assert(
    parse.Int3("1110111") == (7, "0111")
  )
  assert(
    parse.LiteralVal("101111111000101000") == (2021, "000")
  )

  assert(parse._Packet("110100101111111000101000")._1 == Packet.Literal(6, 2021))

  // println(parse._Packet("00111000000000000110111101000101001010010001001000000000"))

  assert(versionSumFromHex("8A004A801A8002F478") == 16)
  assert(versionSumFromHex("620080001611562C8802118E34") == 12)
  assert(versionSumFromHex("C0015000016115A2E0802F182340") == 23)
  assert(versionSumFromHex("A0016C880162017C3686B18A3D4780") == 31)

  println(versionSumFromHex(hex))
  println(resultFromHex(hex))
}
