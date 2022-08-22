object stuff {

  /*
   a b c d e f g
       h   i
       j   k
   */

  val ADJ = Map[Char, String](
    'a' -> "b",
    'b' -> "ac",
    'c' -> "bdh",
    'd' -> "ce",
    'e' -> "dfi",
    'f' -> "eg",
    'g' -> "f",
    'h' -> "cj",
    'j' -> "h",
    'i' -> "ek",
    'k' -> "i"
  )

  val HOMES = Map[Int, String](
    0 -> "hj",
    1 -> "hj",
    2 -> "ik",
    3 -> "ik"
  )
  val SUPER_HOMES = Map[Int, Char](
    0 -> 'j',
    1 -> 'j',
    2 -> 'k',
    3 -> 'k'
  )
  val EXITS = "ce"

  val ALL_HOMES = "hjik".toSet

  val COAMPH = Map[Int, Int](
    0 -> 1,
    1 -> 0,
    2 -> 3,
    3 -> 2
    // 4 -> 5,
    // 5 -> 4,
    // 6 -> 7,
    // 7 -> 6
  )

  val COSTS = Map[Int, Long](
    0 -> 1L,
    1 -> 1L,
    2 -> 1L,
    3 -> 1L
  )

  def isComplete(s: String): Boolean =
    (0 until s.length).foldLeft(true) {
      case (acc, i) =>
        acc && isSafelyHome(s, i)
    }

  def childStates(state: (String, Long)): Set[(String, Long)] = state match {
    case (s, cost) =>
      (0 until s.length).foldLeft(Set.empty[(String, Long)]) { (acc, i) =>
        // if (isSafelyHome(s, i)) acc
        // else
        acc ++ sucs(s, i).map {
          case (pos, moves) => s.updated(i, pos) -> (cost + moves * COSTS(i))
        }.toSet
      }
  }

  def sucs(s: String, i: Int): Set[(Char, Int)] =
    if (isSafelyHome(s, i)) {
      println("is safely home")
      Set((s(i) -> 0))
    } else if (ALL_HOMES.contains(s(i))) {
      println("from room")
      succsForRoom(s, i)
    } else {
      println("from hall")
      homeFromHall(s, i)
    }

  def succsForRoom(s: String, i: Int): Set[(Char, Int)] =
    (validSuccs(s, i).filter { p =>
      val isAnExit = EXITS.contains(p._1)
      // A position is not valid if it is a home position but it's not "safe"
      val isSH            = isSafelyHome(s.replace(s(i), p._1), i)
      val isHomeButUnsafe = HOMES(i).contains(p._1) && !isSH

      !(isAnExit || isHomeButUnsafe)
    }) + (s(i) -> 0) // the starting position is allowed to be invalid

  def homeFromHall(s: String, i: Int): Set[(Char, Int)] = {
    // println("homeFromHall")
    validSuccs(s, i).filter { p =>
      // println(s"amph $i pos $p")
      // println(s"$s -> ${s.replace(s(i), p._1)}")
      val isSH = isSafelyHome(s.replace(s(i), p._1), i)
      // println(s"isSafelyHome: $isSH")
      isSH
    }
  }

  def isSafelyHome(s: String, i: Int): Boolean = {
    val isSuperHome                = SUPER_HOMES(i) == s(i)
    val isHomeAndCoAmphIsSuperHome = (HOMES(i).contains(s(i)) && SUPER_HOMES(i) == s(COAMPH(i)))
    isSuperHome || isHomeAndCoAmphIsSuperHome
  }

  def validSuccs(s: String, i: Int): Set[(Char, Int)] = {
    val otherHomes = ALL_HOMES -- HOMES(i)
    val occupied   = s.toSet
    val invalid    = occupied

    reachable(s, List.empty, Set.empty, List(s(i) -> 0), occupied)
      .filter(p => !(EXITS.contains(p._1) || otherHomes.contains(p._1)))
  }

  def reachable(
    s: String,
    seen: List[(Char, Int)],
    seenSet: Set[Char],
    unseen: List[(Char, Int)],
    invalid: Set[Char]
  ): Set[(Char, Int)] =
    unseen match {
      case Nil => seen.toSet
      case (h @ (pos, moves)) :: t =>
        reachable(
          s,
          seen :+ h,
          seenSet + pos,
          t ++ (ADJ(pos)
            .filter(!seenSet.contains(_))
            .filter(!invalid.contains(_))
            .map(p => p -> (moves + 1))),
          invalid
        )
    }

  type S = String

  def runState(s: S): Option[(S, Long)] = {
    val x: (Set[(S, Long)], Map[S, Long])                                       = Set(s -> 0L) -> Map.empty[S, Long]
    val none: Option[((S, Long), (Set[(S, Long)], Map[S, Long], Map[S, Long]))] = None

    LazyList
      .unfold[Option[(S, Long)], (Set[(S, Long)], Map[S, Long], Option[(S, Long)])](
        (Set(s -> 0L), Map.empty[S, Long], None)
      ) {
        case (states, bests, bestComplete) =>
          val newStates = states
            .flatMap {
              childStates
            }

          val bestNewStates = newStates
            .filter {
              case (state, cost) =>
                bests.get(state) match {
                  case Some(bestCost) if bestCost < cost => false
                  case _                                 => true
                }
            }

          val updatedBests = bests ++ bestNewStates.toMap
          val bestNewComplete =
            newStates.filter(s => isComplete(s._1)).toList.sortBy(_._2).reverse.headOption
          println(s"bestComplete: $bestComplete")
          val updatedComplete = (bestComplete, bestNewComplete) match {
            case (a @ Some((_, ca)), b @ Some((_, cb))) => if (ca < cb) a else b
            case (a @ Some(_), None)                    => a
            case (None, b @ Some(_))                    => b
            case _                                      => None
          }

          println(s"newStates: ${newStates.size}; bests.size: ${bests.size}")

          Option.when(!newStates.isEmpty) {
            updatedComplete -> (states ++ bestNewStates, updatedBests, updatedComplete)
          }
      }
      .dropWhile(_.isEmpty)
      .head
  }
}

@main
def run() = {
  import stuff._

  /*
   a b c d e f g
       h   i
       j   k
   */

  // {
  //   val s = "hjik"; println(runState(s))
  // }

  // {
  //   val s = "ikhj"; println(runState(s))
  // }

  {
    val s = "abhj"; println(runState(s))
  }

  // {
  //   val s = "abhj"; println(childStates(s -> 0L))
  // }
  {
    // val s = "abkj"; // println(childStates(s -> 5L))
    // println(sucs(s, 3))
    // println(validSuccs(s, 3))
  }
}
