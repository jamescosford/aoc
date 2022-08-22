// Ammonite 2.4.0, Scala 2.13.6

import scala.io.Source

/*
  [ a| b| c| d| e| f| g| h| i| j| k]
        | l|  | n|  | p|  | r|
        | m|  | o|  | q|  | s|
        | t|  | u|  | v|  | w|
        | x|  | y|  | z|  | !|
 */

object stuff {

  type S = String

  /*
  [ a| b| c| d| e| f| g| h| i| j| k]
        | l|  | n|  | p|  | r|
        | m|  | o|  | q|  | s|
        | t|  | u|  | v|  | w|
        | x|  | y|  | z|  | !|
   */

  val ADJ = Map[Char, String](
    'a' -> "b",
    'b' -> "ac",
    'c' -> "bdl",
    'd' -> "ce",
    'e' -> "dnf",
    'f' -> "eg",
    'g' -> "fph",
    'h' -> "gi",
    'i' -> "hrj",
    'j' -> "ik",
    'k' -> "j",
    'l' -> "cm",
    'm' -> "lt",
    'n' -> "eo",
    'o' -> "nu",
    'p' -> "gq",
    'q' -> "pv",
    'r' -> "is",
    's' -> "rw",
    't' -> "mx",
    'x' -> "t",
    'u' -> "oy",
    'y' -> "u",
    'v' -> "qz",
    'z' -> "v",
    'w' -> "s!",
    '!' -> "w"
  )

  /*
  [ a| b| c| d| e| f| g| h| i| j| k]
        | l|  | n|  | p|  | r|
        | m|  | o|  | q|  | s|
        | t|  | u|  | v|  | w|
        | x|  | y|  | z|  | !|
   */

  val HOMES = Map[Int, String](
    0  -> "lmtx",
    1  -> "lmtx",
    2  -> "lmtx",
    3  -> "lmtx",
    4  -> "nouy",
    5  -> "nouy",
    6  -> "nouy",
    7  -> "nouy",
    8  -> "pqvz",
    9  -> "pqvz",
    10 -> "pqvz",
    11 -> "pqvz",
    12 -> "rsw!",
    13 -> "rsw!",
    14 -> "rsw!",
    15 -> "rsw!"
  )

  val AS = Set(0, 1, 2, 3)
  val BS = Set(4, 5, 6, 7)
  val CS = Set(8, 9, 10, 11)
  val DS = Set(12, 13, 14, 15)

  val COAMPH = Map[Int, Set[Int]](
    0  -> (AS - 0),
    1  -> (AS - 1),
    2  -> (AS - 2),
    3  -> (AS - 3),
    4  -> (BS - 4),
    5  -> (BS - 5),
    6  -> (BS - 6),
    7  -> (BS - 7),
    8  -> (CS - 8),
    9  -> (CS - 9),
    10 -> (CS - 10),
    11 -> (CS - 11),
    12 -> (DS - 12),
    13 -> (DS - 13),
    14 -> (DS - 14),
    15 -> (DS - 15)
  )

  /*
  [ a| b| c| d| e| f| g| h| i| j| k]
        | l|  | n|  | p|  | r|
        | m|  | o|  | q|  | s|
        | t|  | u|  | v|  | w|
        | x|  | y|  | z|  | !|
   */

  val ALL_HOMES = "lmnopqrstuvwxyz!".toSet

  val EXITS = "cegi"

  val COSTS = Map[Int, Long](
    0  -> 1L,
    1  -> 1L,
    2  -> 1L,
    3  -> 1L,
    4  -> 10L,
    5  -> 10L,
    6  -> 10L,
    7  -> 10L,
    8  -> 100L,
    9  -> 100L,
    10 -> 100L,
    11 -> 100L,
    12 -> 1000L,
    13 -> 1000L,
    14 -> 1000L,
    15 -> 1000L
  )

  // If they are in the hall, any path that doesn't lead to home is invalid
  // def succsFromRoom(s: S, i: Int): Set[(Int, Int)] = // Anywhere

  // If in a room it will generate a state for all reachable spaces in the hall, OR home, OR staying put
  // If in the hall it will only go home

  // override def unfold[A, S](init: S)(f: S => Option[(A, S)]): LazyList[A] =
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
            newStates.filter(s => isComplete(s._1)).toList.sortBy(_._2).headOption
          println("complete: " + bestComplete)
          val updatedComplete = (bestComplete, bestNewComplete) match {
            case (a @ Some((_, ca)), b @ Some((_, cb))) => if (ca < cb) a else b
            case (a @ Some(_), None)                    => a
            case (None, b @ Some(_))                    => b
            case _                                      => None
          }
          // val updatedComplete = bestNewComplete

          println(s"newStates: ${newStates.size}; bests.size: ${bests.size}")

          Option.when(!newStates.isEmpty) {
            updatedComplete -> (states ++ bestNewStates, updatedBests, updatedComplete)
          }

        // Some(bestNewComplete -> (newStates, updatedBests, bestNewComplete))
      }
      .map(x => { println(x); x })
      .dropWhile(_.isEmpty)
      .drop(20)
      .head
  }

  def isComplete(s: S): Boolean =
    (0 until s.length).foldLeft(true) {
      case (acc, i) =>
        acc && isSafelyHome(s, i)
    }

  def childStates(state: (S, Long)): Set[(S, Long)] = state match {
    case (s, cost) =>
      (0 until s.length).foldLeft(Set.empty[(S, Long)]) { (acc, i) =>
        // if (isSafelyHome(s, i)) acc
        // else
        acc ++ sucs(s, i).map {
          case (pos, moves) => s.updated(i, pos) -> (cost + moves * COSTS(i))
        }.toSet
      }
  }

  def sucs(s: S, i: Int): Set[(Char, Int)] =
    if (isSafelyHome(s, i)) {
      Set((s(i) -> 0))
    } else if (ALL_HOMES.contains(s(i))) {
      succsForRoom(s, i)
    } else {
      homeFromHall(s, i)
    }

  /**
   * @param s the current world state
   * @param i the index of the current amphipod in the state
   * @return all accessible positions for the specified amphipod
   */
  def succsForRoom(s: String, i: Int): Set[(Char, Int)] =
    (validSuccs(s, i).filter { p =>
      val isAnExit = EXITS.contains(p._1)
      // A position is not valid if it is a home position but it's not "safe"
      val isSH            = isSafelyHome(s.replace(s(i), p._1), i)
      val isHomeButUnsafe = HOMES(i).contains(p._1) && !isSH

      !(isAnExit || isHomeButUnsafe)
    }) + (s(i) -> 0) // the starting position is allowed to be invalid

  // Find all reachable positions, and if any of them are "home" then generate a new state with that amphipod at home
  def homeFromHall(s: S, i: Int): Set[(Char, Int)] =
    validSuccs(s, i).filter { p =>
      isSafelyHome(s.replace(s(i), p._1), i)
    }

  def isSafelyHome(s: S, i: Int): Boolean = {
    if (!HOMES(i).contains(s(i))) {
      false
    } else {
      // if all the cells below me contain my co-amphs
      val belowMe         = HOMES(i).dropWhile(_ != s(i)).drop(1).toSet
      val whereMyHomiesAt = COAMPH(i).map(s(_))
      belowMe.intersect(whereMyHomiesAt).size == belowMe.size
    }
  }

  // def isSafelyHome(s: S, i: Int): Boolean = {
  //   val isSuperHome                = SUPER_HOMES(i) == s(i) // you can't get to super home with another amph in the way
  //   val isHomeAndCoAmphIsSuperHome = (HOMES(i).contains(s(i)) && SUPER_HOMES(i) == s(COAMPH(i)))

  //   isSuperHome || isHomeAndCoAmphIsSuperHome
  // }

  def validSuccs(s: String, i: Int): Set[(Char, Int)] = {
    val otherHomes = ALL_HOMES -- HOMES(i)
    val occupied   = s.toSet
    val invalid    = occupied

    reachable(s, List.empty, Set.empty, List(s(i) -> 0), occupied)
      .filter(p => !(EXITS.contains(p._1) || otherHomes.contains(p._1)))
  }

  /**
   * @param s the current world state
   * @param seen the positions in the world which have already been visited, and their costs
   * @param seenSet the set of positions already seen
   * @param unseen positions in the world we have discovered but not yet explored
   * @param invalid positions in the world which will block the path of this amphipod
   * @return set of accessible positions, and their associated moves
   */
  def reachable(
    s: S,
    seen: List[(Char, Int)],
    seenSet: Set[Char],
    unseen: List[(Char, Int)],
    invalid: Set[Char]
  ): Set[(Char, Int)] = {
    // println(seenSet.toList.sorted)
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
  }

}

@main
def run() = {
  import stuff._

  /*
  [ a| b| c| d| e| f| g| h| i| j| k]
        | l|  | n|  | p|  | r|
        | m|  | o|  | q|  | s|
        | t|  | u|  | v|  | w|
        | x|  | y|  | z|  | !|
   */
// #############
// #...........#
// ###D#D#B#A###
//   #D#C#B#A#
//   #D#B#A#C#
//   #B#C#A#C#
//   #########

  val as = "vzrs"
  val bs = "xupq"
  val cs = "yow!"
  val ds = "lmtn"

  val s = as + bs + cs + ds

  // val s = IndexedSeq(
  //   // a  a  b  b  c  c  d  d
  //   'm', 's', 'l', 'p', 'n', 'q', 'o', 'r'
  // ).mkString

  // println(isComplete(s))
  println(runState(s))

  // println(printState(s))

}
