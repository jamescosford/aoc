// Ammonite 2.4.0, Scala 2.13.6
import scala.io.Source
/*

inp w
mul x 0 // clear x
add x z // set x to old result
mod x 26
div z 1 // no-op
add x 13 // add 13 to x
eql x w // 1 if x == the input
eql x 0 // 1 if x == 0
mul y 0 // clear y
add y 25 // set y to 25
mul y x // 25 if x was equal to 0, or 0
add y 1 // 26 or 1
mul z y // mul old result by y
mul y 0 // clear y
add y w // set y = input
add y 3 // add 3 to it
mul y x // multiply by x (1 or 0)
add z y


z is the only value propagated

model it as a single equations with z0, z1, z2 ... z13

each section is a function of (input, z_prev) => z

by working backwards we can find what combination of (input, z_prev) results in
z == 0

 */

object stuff {

  sealed trait Operand
  case class IntVal(v: Int) extends Operand
  object IntVal {
    implicit def int2IntVal(x: Int) = IntVal(x)
  }
  sealed trait Register extends Operand {
    val i: Int
  }
  object Register {
    def fromChar(c: String): Register =
      c match {
        case "w" => W
        case "x" => X
        case "y" => Y
        case "z" => Z
      }
  }
  case object W extends Register {
    override val i: Int = 0
  }
  case object X extends Register {
    override val i: Int = 1
  }
  case object Y extends Register {
    override val i: Int = 2
  }
  case object Z extends Register {
    override val i: Int = 3
  }

  type State = (Seq[Int], IndexedSeq[Int])

  sealed trait Op

  case class inp(to: Register) extends Op

  case class pri(v: Register) extends Op

  sealed trait BinOp extends Op {
    val A: Register
    val B: Operand
    def run(a: Int, b: Int): Int
  }
  object BinOp {
    def fromString(s: String, A: Register, B: Operand): BinOp =
      s match {
        case "mul" => mul(A, B)
        case "add" => add(A, B)
        case "div" => div(A, B)
        case "mod" => mod(A, B)
        case "eql" => eql(A, B)
      }
  }

  case class mul(A: Register, B: Operand) extends BinOp {
    override def run(a: Int, b: Int): Int = a * b
  }
  case class add(A: Register, B: Operand) extends BinOp {
    override def run(a: Int, b: Int): Int = a + b
  }

  case class mod(A: Register, B: Operand) extends BinOp {
    override def run(a: Int, b: Int): Int = a % b
  }
  
  case class div(A: Register, B: Operand) extends BinOp {
    override def run(a: Int, b: Int): Int = a / b
  }

  case class eql(A: Register, B: Operand) extends BinOp {
    override def run(a: Int, b: Int): Int = if (a == b) 1 else 0
  }

  def runOps(ops: List[Op], s: State): State =
    ops.foldLeft(s) {
      case (s, op) =>
        op match {
          case pri(v) => println(s"pri: $v ${s._2(v.i)}"); s
          case inp(to) => s.copy(_1 = s._1.tail, _2 = s._2.updated(to.i, s._1.head))
          case bo: BinOp =>
            val bi = bo.B match {
              case IntVal(i) => i
              case r: Register => s._2(r.i)
            }
            val v = bo.run(s._2(bo.A.i), bi)
            s.copy(_2 = s._2.updated(bo.A.i, v))
        }
    }

  // list of (Z, Input) pairs which give the desired result
  def findSolns(ops: List[Op], forZ: Int): List[(Int, Int)] =
    (for {
      i <- (1 to 9)
      z <- (-200 to 200)
    } yield (z, i)).map { case (z, i) =>
      (z, i) -> runOps(ops, Seq(i) -> IndexedSeq(0,0,0,z))
    }.filter(_._2._2(3) == forZ)
    .map {_._1}.toList

  def solvBackward(ops: Seq[List[Op]]) =
    ops.foldLeft[(List[Map[Int, Int]], Set[Int])](List.empty -> Set(0)) {
      case ((res, zs), ops) =>
        val solns = zs.flatMap(z => findSolns(ops, z))
        (
          res :+ solns.toMap,
          solns.map(_._1).toSet
        )
    }

  def forward(ops: List[Op], forZ: Int): List[(Int, (Int, Int))] =
    (1 to 9).map { i =>
      val res = runOps(ops, Seq(i) -> IndexedSeq(0,0,0,forZ))
      val z = res._2(3)
      (i,(forZ, z))
    }.toList

  def solvForward(ops: Seq[List[Op]]) =
    ops.foldLeft[(List[Map[Int, (Int, Int)]], Set[Int])](List.empty -> Set(0)) {
     case ((res, zs), ops) =>
      val solns = zs.flatMap(z => forward(ops, z))
      (
        res :+ solns.toMap,
        solns.map(_._2._2).toSet
      )
    }
  
    


  def parse(s: String): Op = {
    val inpRx = raw"inp ([wxyz])".r
    val priRx = raw"pri ([wxyz])".r
    val opRxA = raw"([a-z]{3}) ([wxyz]) ([wxyz])".r
    val opRxB = raw"([a-z]{3}) ([wxyz]) ([-]*[0-9]+)".r
    s match {
      case priRx(c) => pri(Register.fromChar(c))
      case inpRx(c) => inp(Register.fromChar(c))
      case opRxA(cmd, reg, v) => BinOp.fromString(cmd, Register.fromChar(reg), Register.fromChar(v))
      case opRxB(cmd, reg, v) => BinOp.fromString(cmd, Register.fromChar(reg), IntVal(v.toInt))
    }
  }

  def parseAll(s: String): List[Op] =
    s.split('\n').map(parse).toList


  def MONADBlock(A: Int, B: Int, C: Int)(w: Int, z_1: Int): (Int, Int, Int, Int) = {
    var x = z_1
    x = x % 26
    x = x + B
    x = if (x == w) 1 else 0
    x = if (x == 0) 1 else 0
    var z = z_1 / A
    var y = (25 * x + 1)
    z = z * y
    y = (w + C) * x
    z = z + y
    z
    (w, x, y, z)
  }

  def toMonadBlocks(ops: IndexedSeq[List[Op]]): List[(Int, Int) => (Int, Int, Int, Int)] =
    ops.foldLeft[List[(Int, Int) => (Int, Int, Int, Int)]](List.empty) {
      case (acc, ops) =>
        val is = ops.toIndexedSeq
        acc :+ MONADBlock(
          is(4) match {
            case b: BinOp => b.B match { case IntVal(v) => v }
          },
          is(5) match {
            case b: BinOp => b.B match { case IntVal(v) => v }
          },
          is(15) match {
            case b: BinOp => b.B match { case IntVal(v) => v }
          }
        )
    }
}

@main
def run() = {
  import stuff._
  import IntVal.int2IntVal

  val s = Seq(7) -> IndexedSeq(0,0,0,0)
  val res = runOps(
    List(
      inp(W),
      add(Z, W),
      mod(Z, 2),
      div(W, 2),
      add(Y, W),
      mod(Y, 2),
      div(W, 2),
      add(X, W),
      mod(X, 2),
      div(W, 2),
      mod(W, 2)
    ),
    s
  )

  val input = """inp w
mul x 0
add x z
mod x 26
div z 1
add x 13
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 3
mul y x
add z y"""

  val ops = parseAll(input)

  findSolns(ops, 0).foreach(println)
  // Using this we know what we want the answer for the previous block to be; 
  // we want to find the highest input for the previous block which generates
  // values in the z domain discovered for this block

  // (1 to 9).map { i =>
  //   i -> runOps(ops, Seq(i) -> IndexedSeq(0,0,0,0))
  // }.foreach(println)

    val data = Source
    .fromFile("2021/day_024_input.txt")
    .getLines()
    .map(parse)
    .grouped(18)
    .map(_.toList)
    .toIndexedSeq

    solvBackward(data.reverse)._1.map { zs =>
      zs.toList.sortBy(_._1)
    }.zipWithIndex.map { case (a, b) => b -> a}.foreach(println)

    // solvForward(data.take(3))._2.toIndexedSeq.sorted.foreach(println)

    // forward(data(0), 0).foreach(println)

    (0 to 9).map { w =>
      w -> MONADBlock(1, 13, 3)(w, 0)
    }.foreach(println)

    // (0 to 9).map { w =>
    //   w -> toMonadBlocks(data)(0)(w, 0)
    // }.foreach(println)

    // (0 to 9).map { w =>
    //   w -> MONADBlock(1, 11, 12)(w, 6)
    // }.foreach(println)




}


