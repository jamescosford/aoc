// Ammonite 2.4.0, Scala 2.13.6

import scala.io.Source

/*
  [ a| b| c| d| e| f| g| h| i| j| k]
        | l|  | n|  | p|  | r|
        | m|  | o|  | q|  | s|
 */

object TestReachable {
  import stuff._

  def testReachable = {
    {
      /*
#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########
       */

      val s = "mslpqnor"

      val i          = 2
      val otherHomes = ALL_HOMES -- HOMES(i)
      val occupied   = s.toSet
      val invalid    = otherHomes ++ occupied

      // we are looking at the reachable positions for the B in position 11

      // println(
      //   stuff.reachable(s, List.empty, Set.empty, List(s(i) -> 0), invalid).toList.sortBy(_._1)
      // )

      // succsForRoom(s, i).foreach(s => println(printState(s.)))
      println("#############")
      println(printState(s))
      childStates(s, 0).foreach(s => println(printState(s._1)))
    }
    {
      /*
  [ a| b| c| d| e| f| g| h| i| j| k]
        | l|  | n|  | p|  | r|
        | m|  | o|  | q|  | s|
       */
      /*
#############
#B..C......#
###.#.#B#D###
  #A#D#C#A#
  #########
       */
      val s = "msapdqor"

      val i          = 2
      val otherHomes = ALL_HOMES -- HOMES(i)
      val occupied   = s.toSet
      val exits      = EXITS.toSet
      val invalid    = otherHomes ++ occupied

      val expected = Set('a' -> 0, 'b' -> 1, 'c' -> 2)

      // we are looking at the reachable positions for the B in position 11
      // should be positions a, b
      val res =
        stuff.reachable(s, List.empty, Set.empty, List(s(i) -> 0), invalid).toList.sortBy(_._1)

      println(
        stuff.reachable(s, List.empty, Set.empty, List(s(i) -> 0), invalid).toList.sortBy(_._1)
      )
      assert(expected.diff(res.toSet) == Set.empty)
    }

    {
      /*
  [ a| b| c| d| e| f| g| h| i| j| k]
        | l|  | n|  | p|  | r|
        | m|  | o|  | q|  | s|
       */
      /*
#############
#B..C......#
###.#.#B#D###
  #A#D#C#A#
  #########
       */
      val s = "msapdqor"

      val i          = 3
      val otherHomes = ALL_HOMES -- HOMES(i)
      val occupied   = s.toSet
      val invalid    = otherHomes ++ occupied

      val expected = Set('f' -> 2, 'g' -> 1, 'h' -> 2, 'i' -> 3, 'j' -> 4, 'k' -> 5)

      // we are looking at the reachable positions for the B in position p
      // should be positions a, b
      val res =
        stuff.reachable(s, List.empty, Set.empty, List(s(i) -> 0), invalid).toList.sortBy(_._1)

      println(
        stuff.reachable(s, List.empty, Set.empty, List(s(i) -> 0), invalid).toList.sortBy(_._1)
      )
      assert(expected.diff(res.toSet) == Set.empty)
    }
  }
  /*
  [ a| b| c| d| e| f| g| h| i| j| k]
        | l|  | n|  | p|  | r|
        | m|  | o|  | q|  | s|
   */
  /*
    #############
    #...........#
    ###B#C#B#D###
      #A#D#C#A#
      #########
   */

  def testSuccsForRoom = {
    val s = IndexedSeq(
      // a  a  b  b  c  c  d  d
      'm', 's', 'l', 'p', 'n', 'q', 'o', 'r'
    ).mkString

    println(succsForRoom(s, 2).toList.sortBy(_._1))
    println(succsForRoom(s, 6).toList.sortBy(_._1))
    println(succsForRoom(s, 7).toList.sortBy(_._1))
  }

  /*
  [ a| b| c| d| e| f| g| h| i| j| k]
        | l|  | n|  | p|  | r|
        | m|  | o|  | q|  | s|
   */
  /*
    #############
    #B........CD#
    ###.#.#B#D###
      #A#.#C#A#
      #########
   */
  def testHomeFromHall = {
    val s = "msapjqkr"
    println(homeFromHall(s, 2))
    assert(homeFromHall(s, 2).diff(Set('o' -> 6)) == Set.empty)

    println(homeFromHall(s, 6))
    println(homeFromHall(s, 0))
  }
}

object stuff {

  type S = String

  /*
  [ a| b| c| d| e| f| g| h| i| j| k]
        | l|  | n|  | p|  | r|
        | m|  | o|  | q|  | s|
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
    'm' -> "l",
    'n' -> "eo",
    'o' -> "n",
    'p' -> "gq",
    'q' -> "p",
    'r' -> "is",
    's' -> "r"
  )

  /*
  [ a| b| c| d| e| f| g| h| i| j| k]
        | l|  | n|  | p|  | r|
        | m|  | o|  | q|  | s|
   */

  val HOMES = Map[Int, String](
    0 -> "lm",
    1 -> "lm",
    2 -> "no",
    3 -> "no",
    4 -> "pq",
    5 -> "pq",
    6 -> "rs",
    7 -> "rs"
  )

  val SUPER_HOMES = Map[Int, Char](
    0 -> 'm',
    1 -> 'm',
    2 -> 'o',
    3 -> 'o',
    4 -> 'q',
    5 -> 'q',
    6 -> 's',
    7 -> 's'
  )

  val COAMPH = Map[Int, Int](
    0 -> 1,
    1 -> 0,
    2 -> 3,
    3 -> 2,
    4 -> 5,
    5 -> 4,
    6 -> 7,
    7 -> 6
  )

  val ALL_HOMES = "lmnopqrs".toSet

  val EXITS = "cegi"

  val COSTS = Map[Int, Long](
    0 -> 1L,
    1 -> 1L,
    2 -> 10L,
    3 -> 10L,
    4 -> 100L,
    5 -> 100L,
    6 -> 1000L,
    7 -> 1000L
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

  // #############
  // #...........#
  // ###D#D#B#A###
  //   #B#C#A#C#
  //   #########
  /*
  [ a| b| c| d| e| f| g| h| i| j| k]
        | l|  | n|  | p|  | r|
        | m|  | o|  | q|  | s|
   */
  def printState(s: S): String = {
    //         "abcdefghijk"
    val hall = "..........."
    val ro0m = "##.#.#.#.##"
    val ro1m = "##.#.#.#.##"

    val x = Array[Char]('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.')
    val y = Array[Char](' ', '#', '.', '#', '.', '#', '.', '#', '.', '#', ' ')
    val z = Array[Char](' ', '#', '.', '#', '.', '#', '.', '#', '.', '#', ' ')

    val a = Array[Array[Char]](x, y, z)

    val indices = Map[Char, (Int, Int)](
      'a' -> (0, 0),
      'b' -> (0, 1),
      'c' -> (0, 2),
      'd' -> (0, 3),
      'e' -> (0, 4),
      'f' -> (0, 5),
      'g' -> (0, 6),
      'h' -> (0, 7),
      'i' -> (0, 8),
      'j' -> (0, 9),
      'k' -> (0, 10),
      'l' -> (1, 2),
      'm' -> (2, 2),
      'n' -> (1, 4),
      'o' -> (2, 4),
      'p' -> (1, 6),
      'q' -> (2, 6),
      'r' -> (1, 8),
      's' -> (2, 8)
    )
    val iToC = Map[Int, Char](
      0 -> 'a',
      1 -> 'A',
      2 -> 'b',
      3 -> 'B',
      4 -> 'c',
      5 -> 'C',
      6 -> 'd',
      7 -> 'D'
    )

    s.zipWithIndex.foreach {
      case (c, i) =>
        val ix = indices(c)
        val cx = iToC(i)
        a(ix._1)(ix._2) = cx
    }

    "#" + a(0).mkString + "#\n " + a(1).mkString + " \n " + a(2).mkString

  }

  def isSafelyHome(s: S, i: Int): Boolean = {
    val isSuperHome                = SUPER_HOMES(i) == s(i) // you can't get to super home with another amph in the way
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

  // TestReachable.testSuccsForRoom
  // TestReachable.testReachable
  TestReachable.testHomeFromHall

  // val init = parse(
  //   Source
  //     .fromFile("day_023_test_input.txt")
  //     // .fromFile("day_022_input.txt")
  //     .getLines()
  //     .toList
  // )
  // val _init = init.copy(as = init.as.map(a => initMS(a, init)))

  // println(_init)

  // val s = IndexedSeq(
  //   // a  a  b  b  c  c  d  d
  //   16, 17, 12, 15, 14, 18, 11, 13
  // )

  /*
  [ a| b| c| d| e| f| g| h| i| j| k]
        | l|  | n|  | p|  | r|
        | m|  | o|  | q|  | s|
   */

  // val s = IndexedSeq(
  //   // a  a  b  b  c  c  d  d
  //   12, 18, 11, 15, 13, 16, 14, 17
  // )

  // val s = IndexedSeq(
  //   // a  a  b  b  c  c  d  d
  //   11, 12, 13, 14, 15, 16, 17, 18
  // )

// #############
// #...........#
// ###D#D#B#A###
//   #B#C#A#C#
//   #########

  val s = "qrmposln"

  // val s = IndexedSeq(
  //   // a  a  b  b  c  c  d  d
  //   'm', 's', 'l', 'p', 'n', 'q', 'o', 'r'
  // ).mkString

  // println(isComplete(s))
  println(runState(s))

  // println(printState(s))

}
