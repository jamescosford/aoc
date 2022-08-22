object stuff {

  /*
   a b c d e
     f   g
   */

  val ADJ = Map[Char, String](
    'a' -> "b",
    'b' -> "acf",
    'c' -> "bd",
    'd' -> "ceg",
    'e' -> "d",
    'f' -> "b",
    'g' -> "d"
  )

  val HOMES = Map[Int, String](
    0 -> "f",
    1 -> "g"
  )
  val EXITS = "bd"

  val ALL_HOMES = "fg".toSet

  val COSTS = Map[Int, Long](
    0 -> 1L,
    1 -> 1L
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
    if (ALL_HOMES.contains(s(i)))
      succsForRoom(s, i)
    else
      homeFromHall(s, i)

  def succsForRoom(s: String, i: Int): Set[(Char, Int)] =
    (validSuccs(s, i).filter { p =>
      val isAnExit = EXITS.contains(p._1)
      // A position is not valid if it is a home position but it's not "safe"
      val isHomeButUnsafe = HOMES(i).contains(p._1) && !isSafelyHome(s, i)

      !(isAnExit || isHomeButUnsafe)
    }) + (s(i) -> 0) // the starting position is allowed to be invalid

  def homeFromHall(s: String, i: Int): Set[(Char, Int)] = {
    println("homeFromHall")
    validSuccs(s, i).filter { p =>
      println(s"amph $i pos $p")
      println(s"$s -> ${s.replace(s(i), p._1)}")
      val isSH = isSafelyHome(s.replace(s(i), p._1), i)
      println(s"isSafelyHome: $isSH")
      isSH
    }
  }

  def isSafelyHome(s: String, i: Int): Boolean =
    HOMES(i).contains(s(i))

  def validSuccs(s: String, i: Int): Set[(Char, Int)] = {
    val otherHomes = ALL_HOMES -- HOMES(i)
    val occupied   = s.toSet
    val invalid    = otherHomes ++ occupied

    reachable(s, List.empty, Set.empty, List(s(i) -> 0), invalid).filter(p => !EXITS.contains(p._1))
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
   a b c d e
     f   g
   */

  {
    // val s = "ae"; println(runState(s))
  }

  {
    // val s = "ea"; println(runState(s))
  }

  {
    val s = "gf"; println(runState(s))
  }

  {
    // val s = "fg"; println(runState(s))
  }

}
