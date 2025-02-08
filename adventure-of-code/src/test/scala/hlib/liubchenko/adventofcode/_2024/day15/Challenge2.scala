package hlib.liubchenko.adventofcode._2024.day15

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge2 extends AnyWordSpec with Matchers {
  def moveBoxes(input: Array[String]): Int = {
    val (mapLines, commandsLines) = input.span(_.nonEmpty)
    val map = mapLines.map(_.flatMap {
      case '@' => s"@."
      case 'O' => s"[]"
      case c   => s"$c$c"
    }.toArray)
    val commands = commandsLines.tail.flatMap(_.toArray)
    val (height, width) = (map.length, map.head.length)
    var (robotI, robotJ) = (for { i <- 0 until height; j <- 0 until width if map(i)(j) == '@' } yield (i, j)).head

    def printMap(): Unit = map.map(_.mkString).foreach(println)
    printMap()
    println(s"robot[$robotI, $robotJ]")

    def move(command: Char): Unit = {
      def moveHead(iChange: Int, jChange: Int): Unit = {
        map(robotI)(robotJ) = '.'
        map(robotI + iChange)(robotJ + jChange) = '@'
        robotI += iChange
        robotJ += jChange
      }

      def isBox(v: Char) = Set('[', ']').contains(v)

      def moveHorizontally(jChange: Int): Unit = {
        val next = map(robotI)(robotJ + jChange)

        if (next == '.') moveHead(0, jChange)
        else if (isBox(next)) {
          val nextJ = { if (jChange > 0) robotJ + 1 until width else robotJ - 1 to 0 by -1 }.find { j =>
            !isBox(map(robotI)(j))
          }.get

          if (map(robotI)(nextJ) == '.') {
            val step = if (jChange > 0) -1 else 1
            (nextJ to robotJ by step)
              .sliding(2)
              .foreach { case Seq(a, b) =>
                val tmp = map(robotI)(a); map(robotI)(a) = map(robotI)(b); map(robotI)(b) = tmp
              }
            robotJ += jChange
          }
        }
      }

      def moveVertically(iChange: Int): Unit = {
        val next = map(robotI + iChange)(robotJ)

        def ableToMove(i: Int, j: Int): Boolean = map(i)(j) != '#' && {
          val side =
            if (map(i + iChange)(j) == '[') ableToMove(i + iChange, j + 1)
            else if (map(i + iChange)(j) == ']') ableToMove(i + iChange, j - 1)
            else true

          val main = if (map(i + iChange)(j) != '.') ableToMove(i + iChange, j) else true

          side && main
        }

        def move(i: Int, j: Int): Unit = {
          if (map(i + iChange)(j) == '[') move(i + iChange, j + 1)
          else if (map(i + iChange)(j) == ']') move(i + iChange, j - 1)

          if (map(i + iChange)(j) != '.') move(i + iChange, j)

          map(i + iChange)(j) = map(i)(j)
          map(i)(j) = '.'
        }

        if (next == '.') moveHead(iChange, 0)
        else if (isBox(next) && ableToMove(robotI, robotJ)) {
          move(robotI, robotJ)
          robotI += iChange
        }
      }

      command match {
        case '>' => moveHorizontally(1)
        case '<' => moveHorizontally(-1)
        case 'v' => moveVertically(1)
        case '^' => moveVertically(-1)
      }
    }

    commands.foreach(move)

    (for { i <- 0 until height; j <- 0 until width if map(i)(j) == '[' } yield 100 * i + j).sum
  }

  "Day #15 Challenge #2" should {
    "work as expected #1" in {
      moveBoxes("""#######
          |#...#.#
          |#.....#
          |#.....#
          |#..OO.#
          |#.@O..#
          |#.....#
          |#######
          |
          |>>v>^^^<<^^^>v""".stripMargin.split("\n")) shouldBe 921
    }

    "work as expected #2" in {
      moveBoxes(
        """##########
          |#..O..O.O#
          |#......O.#
          |#.OO..O.O#
          |#..O@..O.#
          |#O#..O...#
          |#O..O..O.#
          |#.OO.O.OO#
          |#....O...#
          |##########
          |
          |<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
          |vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
          |><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
          |<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
          |^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
          |^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
          |>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
          |<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
          |^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
          |v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^""".stripMargin.split("\n")
      ) shouldBe 9021
    }

    "work as expected #3" in {
      val input = Utils.readInputFile(15)
      moveBoxes(input.toArray) shouldBe 1521952
    }
  }
}
