// Scala 2.13.6
import scala.io.Source

sealed trait BT[A]
object BT {
  case class L[A](a: A)               extends BT[A]
  case class N[A](l: BT[A], r: BT[A]) extends BT[A]

  def show(bt: BT[_]): String =
    bt match {
      case L(a)    => a.toString()
      case N(l, r) => "[" + show(l) + "," + show(r) + "]"
    }

  def or[A](a: => Option[A], b: => Option[A]): Option[A] =
    a match {
      case s @ Some(_) => s
      case _           => b
    }

  def leftForkAbove[A](bt: BT[A], stk: List[N[A]]): Option[(BT[A], List[N[A]])] = {
    // println(s"leftForkAbove $bt")
    stk.headOption.flatMap {
      case N(l, r) if bt == r => Some(l -> stk)
      case p @ N(l, r)        => leftForkAbove(p, stk.tail)
    }
  }

  def rightForkAbove[A](bt: BT[A], stk: List[N[A]]): Option[(BT[A], List[N[A]])] = {
    // println(s"rightForkAbove $bt")
    stk.headOption
      .flatMap {
        case N(l, r) if bt == l => Some(r -> stk)
        case p @ N(l, r)        => rightForkAbove(p, stk.tail)
      }
  }

  def rightMostC[A](bt: BT[A], stk: List[N[A]] = List.empty): Option[(L[A], List[N[A]])] =
    bt match {
      case r @ L(_)    => Some(r -> stk)
      case n @ N(l, r) => or(rightMostC(r, n +: stk), rightMostC(l, n +: stk))
    }

  def leftMostC[A](bt: BT[A], stk: List[N[A]] = List.empty): Option[(L[A], List[N[A]])] =
    bt match {
      case r @ L(_)    => Some(r -> stk)
      case n @ N(l, r) => or(leftMostC(l, n +: stk), leftMostC(r, n +: stk))
    }

  def left[A](bt: BT[A], stk: List[N[A]]): Option[(L[A], List[N[A]])] =
    leftForkAbove(bt, stk).flatMap {
      case (n, s) => rightMostC(n, s)
    }
  def right[A](bt: BT[A], stk: List[N[A]]): Option[(L[A], List[N[A]])] =
    rightForkAbove(bt, stk).flatMap {
      case (n, s) => leftMostC(n, s)
    }

  def update[A](old: BT[A], nw: BT[A], stk: List[N[A]]): BT[A] = {
    // println(s"update: old: $old; new: $nw")
    stk.headOption match {
      case Some(n @ N(l, r)) if old == l => update(n, n.copy(l = nw), stk.tail)
      case Some(n @ N(l, r)) if old == r => update(n, n.copy(r = nw), stk.tail)
      case None                          => nw
      case x                             => println(x); ???
    }
  }

  def dfs[A](f: A => Boolean, bt: BT[A], stk: List[N[A]] = List.empty): Option[(L[A], List[N[A]])] =
    bt match {
      case l @ L(a) if f(a) => Some(l, stk)
      case l: L[A]          => None
      case n @ N(l, r) =>
        or(
          dfs(f, l, n +: stk),
          dfs(f, r, n +: stk)
        )
    }

  def dfs_[A](
    f: (BT[A], Int) => Boolean,
    bt: BT[A],
    stk: List[N[A]] = List.empty
  ): Option[(BT[A], List[N[A]])] =
    (bt, stk.length) match {
      case (bt, d) if f(bt, d) => Some(bt, stk)
      case (n @ N(l, r), _) =>
        or(
          dfs_(f, l, n +: stk),
          dfs_(f, r, n +: stk)
        )
      case (L(_), _) => None
    }

  sealed trait Dir
  object Dir {
    case object Left  extends Dir
    case object Right extends Dir

    def toDirStack(v: BT[_], stk: List[N[_]]): List[Dir] = {
      def tds(lst: List[N[_]], acc: List[Dir] = List.empty): List[Dir] = {
        lst match {
          case Nil                     => acc
          case l :: Nil if l.l == v    => tds(Nil, Left +: acc)
          case l :: Nil if l.r == v    => tds(Nil, Right +: acc)
          case p :: c :: t if c == p.l => tds(c :: t, Left +: acc)
          case p :: c :: t if c == p.r => tds(c :: t, Right +: acc)
        }
      }
      tds(stk.reverse)
    }

    def fromDirStack[A](bn: BT[A], stk: List[Dir]): Option[(BT[A], List[N[A]])] = {
      def fds(
        bn: BT[A],
        lst: List[Dir],
        stk: List[N[A]] = List.empty
      ): Option[(BT[A], List[N[A]])] =
        bn match {
          case a @ L(_) if lst.isEmpty    => Some(a -> stk)
          case a @ L(_)                   => None
          case n @ N(_, _) if lst.isEmpty => Some(n -> stk)
          case n @ N(l, r) =>
            lst.head match {
              case Dir.Left  => fds(l, lst.tail, n +: stk)
              case Dir.Right => fds(r, lst.tail, n +: stk)
            }
        }
      fds(bn, stk.reverse)
    }

  }

  def updateLeft[A](f: A => A, of: BT[A], stk: List[N[A]]): Option[BT[A]] =
    left(of, stk).map {
      case (l, ls) =>
        val upd = L(f(l.a))
        update(l, upd, ls)
    }
  def updateRight[A](f: A => A, of: BT[A], stk: List[N[A]]): Option[BT[A]] =
    right(of, stk).map {
      case (r, rs) =>
        val upd = L(f(r.a))
        update(r, upd, rs)
    }

  def tests = {
    {
      val t: BT[Int] = N(L(0), L(1))
      assert(rightMostC(t) == Some(L(1) -> List(t)))
      assert(leftMostC(t) == Some(L(0)  -> List(t)))
    }
    {
      val l         = N(L(0), L(1))
      val r         = N(L(2), L(3))
      val t: N[Int] = N(l, r)
      assert(rightMostC(t) == Some(L(3) -> List(r, t)))
      assert(leftMostC(t) == Some(L(0)  -> List(l, t)))

      assert(left(L(3), List(r, t)) == Some(L(2) -> List(r, t)))
      assert(left(L(2), List(r, t)) == Some(L(1) -> List(l, t)))

      assert(right(L(0), List(l, t)) == Some(L(1) -> List(l, t)))
      assert(right(L(1), List(l, t)) == Some(L(2) -> List(r, t)))

      val r_ = N(L(2), L(4))
      assert(update(L(3), L(4), List(r, t)) == N(l, r_))

      assert(dfs((a: Int) => a == 3, t) == Some(L(3) -> List(r, t)))
      assert(dfs((a: Int) => a > 3, t) == None)

      assert(dfs_[Int]((_, d) => d > 1, t).get._1 == L(0))
      assert(dfs_[Int]((_, d) => d == 1, t).get._1 == l)

      {
        val (n, s) = dfs((a: Int) => a == 3, t).get
        val ds     = Dir.toDirStack(n, s)
        assert(ds == List(Dir.Right, Dir.Right))
        val fds = Dir.fromDirStack(t, ds)
        assert(fds == Some(L(3) -> List(r, t)))
      }
      {
        val (n, s) = dfs((a: Int) => a == 2, t).get
        val ds     = Dir.toDirStack(n, s)
        assert(ds == List(Dir.Left, Dir.Right))
        val fds = Dir.fromDirStack(t, ds)
        assert(fds == Some(L(2) -> List(r, t)))
      }
      {
        val (n, s) = dfs((a: Int) => a == 2, t).get
        val t_     = updateLeft[Int](_ + 5, n, s).get
        assert(t_ == N(N(L(0), L(6)), N(L(2), L(3))))
      }
      {
        val (n, s) = dfs((a: Int) => a == 1, t).get
        val t_     = updateRight[Int](_ + 5, n, s).get
        assert(t_ == N(N(L(0), L(1)), N(L(7), L(3))))
      }

    }

  }

}

object stuff {

  sealed trait Tok
  case class `[`(d: Int) extends Tok
  case object `]`        extends Tok
  case object `,`        extends Tok
  case class D(v: Int)   extends Tok

  sealed trait SN                      extends Tok
  case class Val(v: Int)               extends SN
  case class Num(d: Int, l: SN, r: SN) extends SN

  def toBT(sn: SN): BT[Int] =
    sn match {
      case Num(d, l, r) => BT.N(toBT(l), toBT(r))
      case Val(v)       => BT.L(v)
    }

  def parseTest = {
    println(toBT(parse("[[[[[9,8],1],2],3],4]")._1))
    // println(parse("[7,[6,[5,[4,[3,2]]]]"))
    // println(parse("[[6,[5,[4,[3,2]]]],1]"))
    // println(parse("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"))
  }

  def find(sn: SN, p: List[SN], f: SN => Boolean): Option[List[SN]] =
    sn match {
      case x if f(x) => Some(x +: p)
      case n @ Num(_, l, r) =>
        find(l, n +: p, f) match {
          case None => find(r, n +: p, f)
          case x    => x
        }
    }

  def findTest = {
    val i = parse("[[[[[9,8],1],2],3],4]")._1
    println(find(i, List.empty, _ == Val(9)))
  }

  def firstLeftTest = {
    val i = parse("[[[[[9,8],1],2],3],4]")._1
    println(firstValLeft(List(i), i))
  }

  def firstValLeft(p: List[SN], last: SN): Option[List[SN]] =
    (p.head, last) match {
      case (t @ Num(d, l, r), lst) if r == lst => firstValLeft(t +: p, t)
      case (t @ Num(d, l, r), lst) if l == lst => firstValLeft(p.tail, t)
      case (v: Val, _)                         => Some(v +: p)
      case _                                   => None
    }

  def lastLB(stk: List[Tok]): `[` =
    stk
      .collect[`[`] {
        case x: `[` => x
      }
      .headOption
      .getOrElse(`[`(0))

  val rx_lb = "^\\[(.*)$".r
  val rx_d  = "^(\\d+)(.*)$".r
  val rx_c  = "^,(.*)$".r
  val rx_rb = "^](.*)$".r
  def parse(s: String, toks: List[Tok] = List.empty): (SN, String) = {
    if (s.isEmpty && toks.size == 1)
      return toks.collect { case x: SN => x }.head -> ""
    else
      s match {
        case rx_lb(rem) =>
          parse(rem, `[`(lastLB(toks).d + 1) +: toks)
        case rx_d(d, rem) =>
          parse(rem, Val(d.toInt) +: toks)
        case rx_c(rem) =>
          parse(rem, `,` +: toks)
        case rx_rb(rem) =>
          val (t, ts) = build(toks)
          parse(rem, t +: ts)
        case "" =>
          val (t, ts) = build(toks)
          parse("", t +: ts)
      }
  }

  def build(ts: List[Tok]): (SN, List[Tok]) = {
    val (b, rb) = ts.splitAt(1)
    val (c, rc) = rb.splitAt(1)
    val (a, ra) = rc.splitAt(1)
    val (l, rl) = ra.splitAt(1)
    (for {
      bv <- b.collect { case sn: SN => sn }.headOption
      _  <- c.collect { case `,`    => `,` }.headOption
      av <- a.collect { case sn: SN => sn }.headOption
      lv <- l.collect { case `[`(d) => d }.headOption
    } yield Num(lv, av, bv)).get -> rl
  }

  def expL(n: BT[Int], l: BT.L[Int], stk: List[BT.N[Int]], dirs: List[BT.Dir]): List[BT.N[Int]] = {
    BT.updateLeft[Int](a => a + l.a, n, stk)
      .map {
        BT.Dir.fromDirStack(_, dirs).get._2
      }
      .getOrElse(stk)
  }

  def expR(n: BT[Int], l: BT.L[Int], stk: List[BT.N[Int]], dirs: List[BT.Dir]): List[BT.N[Int]] = {
    BT.updateRight[Int](a => a + l.a, n, stk)
      .map {
        BT.Dir.fromDirStack(_, dirs).get._2
      }
      .getOrElse(stk)
  }

  def explode(ns: (BT[Int], List[BT.N[Int]])): BT[Int] = {
    // println(s"explode $ns")
    val (toExp, toExpS) = ns
    toExp match {
      case n @ BT.N(l, r) =>
        (l, r) match {
          case (l @ BT.L(lv), r @ BT.L(rv)) =>
            val ds   = BT.Dir.toDirStack(toExp, toExpS)
            val stkL = expL(n, l, toExpS, ds)
            val stkR = expR(n, r, stkL, ds)
            BT.update(toExp, BT.L(0), stkR)
          case _ => ???
        }
      case _ => ???
    }
  }

  def explode_(bt: BT[Int]): Option[BT[Int]] =
    BT.dfs_[Int]((n, d) => {
        n match {
          case n @ BT.N(l, r) if d > 3 =>
            (l, r) match {
              case (BT.L(_), BT.L(_)) => true
              case _                  => false
            }
          case _ => false
        }
      }, bt)
      .map { x =>
        println(s"exploding ${x._1}"); x
      }
      .map[BT[Int]](explode)

  def explodeTest = {
    def run(v: String, e: String) = {
      val t = explode_(parse_(v)).get
      println(BT.show(t))
      assert(t == parse_(e))
    }
    // run("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]")
    // run("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]")
    // run("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]")
    // run("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
    // run("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
    // run("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]", "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]")
    // run("[[[[0,7],4],[7,[[8,4],9]]],[1,1]]", "[[[[0,7],4],[15,[0,13]]],[1,1]]")

    run(
      "[[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]],[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]]",
      "[[[[0,[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]],[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]]"
    )
    run(
      "[[[[0,[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]],[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]]",
      "[[[[5,0],[[11,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]],[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]]"
    )
    run(
      "[[[[5,0],[[11,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]],[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]]",
      "[[[[5,11],[0,[13,0]]],[[8,[7,7]],[[7,9],[5,0]]]],[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]]"
    )
    run(
      "[[[[5,11],[0,[13,0]]],[[8,[7,7]],[[7,9],[5,0]]]],[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]]",
      "[[[[5,11],[13,0]],[[8,[7,7]],[[7,9],[5,0]]]],[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]]"
    )
    run(
      "[[[[5,11],[13,0]],[[8,[7,7]],[[7,9],[5,0]]]],[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]]",
      "[[[[5,11],[13,0]],[[15,0],[[14,9],[5,0]]]],[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]]"
    )
    run(
      "[[[[5,11],[13,0]],[[15,0],[[14,9],[5,0]]]],[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]]",
      "[[[[5,11],[13,0]],[[15,14],[0,[14,0]]]],[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]]"
    )
    run(
      "[[[[5,11],[13,0]],[[15,14],[0,[14,0]]]],[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]]",
      "[[[[5,11],[13,0]],[[15,14],[14,0]]],[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]]"
    )
    run(
      "[[[[5,11],[13,0]],[[15,14],[14,0]]],[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]]",
      "[[[[5,11],[13,0]],[[15,14],[14,0]]],[[2,[0,[11,4]]],[[[6,7],1],[7,[1,6]]]]]"
    )
    run(
      "[[[[5,11],[13,0]],[[15,14],[14,0]]],[[2,[0,[11,4]]],[[[6,7],1],[7,[1,6]]]]]",
      "[[[[5,11],[13,0]],[[15,14],[14,0]]],[[2,[11,0]],[[[10,7],1],[7,[1,6]]]]]"
    )
    run(
      "[[[[5,11],[13,0]],[[15,14],[14,0]]],[[2,[11,0]],[[[10,7],1],[7,[1,6]]]]]",
      "[[[[5,11],[13,0]],[[15,14],[14,0]]],[[2,[11,10]],[[0,8],[7,[1,6]]]]]"
    )
    run(
      "[[[[5,11],[13,0]],[[15,14],[14,0]]],[[2,[11,10]],[[0,8],[7,[1,6]]]]]",
      "[[[[5,11],[13,0]],[[15,14],[14,0]]],[[2,[11,10]],[[0,8],[8,0]]]]"
    )
    // split
    run(
      "[[[[5,[5,6]],[13,0]],[[15,14],[14,0]]],[[2,[11,10]],[[0,8],[8,0]]]]",
      "[[[[10,0],[19,0]],[[15,14],[14,0]]],[[2,[11,10]],[[0,8],[8,0]]]]"
    )
    //split
    run(
      "[[[[[5,5],0],[19,0]],[[15,14],[14,0]]],[[2,[11,10]],[[0,8],[8,0]]]]",
      "[[[[0,5],[19,0]],[[15,14],[14,0]]],[[2,[11,10]],[[0,8],[8,0]]]]"
    )
  }

  def splitTest = {
    def run(v: String, e: String) = {
      val t = split_(parse_(v)).get
      println(BT.show(t))
      assert(t == parse_(e))
    }

    run(
      "[[[[5,11],[13,0]],[[15,14],[14,0]]],[[2,[11,10]],[[0,8],[8,0]]]]",
      "[[[[5,[5,6]],[13,0]],[[15,14],[14,0]]],[[2,[11,10]],[[0,8],[8,0]]]]"
    )
    run(
      "[[[[10,0],[19,0]],[[15,14],[14,0]]],[[2,[11,10]],[[0,8],[8,0]]]]",
      "[[[[[5,5],0],[19,0]],[[15,14],[14,0]]],[[2,[11,10]],[[0,8],[8,0]]]]"
    )
    run(
      "[[[[0,5],[19,0]],[[15,14],[14,0]]],[[2,[11,10]],[[0,8],[8,0]]]]",
      "[[[[0,5],[[9,10],0]],[[15,14],[14,0]]],[[2,[11,10]],[[0,8],[8,0]]]]"
    )
  }

  def split(ls: (BT.L[Int], List[BT.N[Int]])): BT[Int] = {
    println(s"split ${ls._1}")
    val ad = ls._1.a.toDouble
    val n  = BT.N(BT.L(math.floor(ad / 2d).toInt), BT.L(math.ceil(ad / 2d).toInt))
    BT.update(ls._1, n, ls._2)
  }

  def split_(bt: BT[Int]): Option[BT[Int]] =
    BT.dfs[Int](_ >= 10, bt)
      .map { x =>
        println(s"splitting ${x._1}"); x
      }
      .map(split)

  def add(a: BT[Int], b: BT[Int]): BT[Int] = {
    val toReduce = BT.N(a, b)
    LazyList
      .unfold[BT[Int], BT[Int]](toReduce) { bt =>
        println(bt)
        BT.or(
          explode_(bt),
          split_(bt)
        ) match {
          case Some(bt_) => Some(bt_ -> bt_)
          case _         => None
        }
      }
      .toList
      .lastOption
      .getOrElse(toReduce)
  }

  def addAll(ss: Iterator[String]): BT[Int] = {
    val sns  = LazyList.from(ss).map(parse_)
    val init = (sns.head, sns.tail)

    LazyList
      .unfold[BT[Int], (BT[Int], LazyList[BT[Int]])](init) {
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

  def parse_(s: String): BT[Int] =
    toBT(parse(s)._1)

  def addTest = {
    def run(a: String, b: String, e: String) = {
      val asn = parse_(a)
      val bsn = parse_(b)
      val esn = parse_(e)
      val rsn = add(asn, bsn)
      println(s"expected: $esn")
      println(s"result  : $rsn")

      assert(rsn == esn)
    }

    // run(
    //   "[[[[4,3],4],4],[7,[[8,4],9]]]",
    //   "[1,1]",
    //   "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
    // )
    // run(
    //   "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
    //   "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
    //   "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"
    // )
    run(
      "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]",
      "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
      "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]"
    )
    // run(
    //   "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]",
    //   "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
    //   "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]"
    // )
    // run(
    //   "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]",
    //   "[7,[5,[[3,8],[1,4]]]]",
    //   "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]"
    // )
    // run(
    //   "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]",
    //   "[[2,[2,2]],[8,[8,1]]]",
    //   "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]"
    // )
    // run(
    //   "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]",
    //   "[2,9]",
    //   "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]"
    // )
    // run(
    //   "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]",
    //   "[1,[[[9,3],9],[[9,0],[0,7]]]]",
    //   "[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]"
    // )
    // run(
    //   "[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]",
    //   "[[[5,[7,4]],7],1]",
    //   "[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]"
    // )
    // run(
    //   "[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]",
    //   "[[[[4,2],2],6],[8,7]]",
    //   "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
    // )
  }
}

@main
def go() {
  import stuff._

  // parseTest
  // firstLeftTest
  // findTest
  // BT.tests

  // addTest
  explodeTest
  splitTest

  // val lines = Source
  //   .fromFile("2021/day_018_test_input.txt")
  //   .getLines()

  // println(addAll(lines))
}
