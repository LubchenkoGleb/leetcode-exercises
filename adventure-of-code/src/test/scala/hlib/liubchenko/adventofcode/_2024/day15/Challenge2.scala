package hlib.liubchenko.adventofcode._2024.day15

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge2 extends AnyWordSpec with Matchers {
  def moveBoxes(input: Array[String]): Int = {
    val (mapLines, commandsLines) = input.span(_.nonEmpty)
    val (map, commands) = (mapLines.map(_.toArray), commandsLines.tail.flatMap(_.toArray))
    val (height, width) = (map.length, map.head.length)

    var (robotI, robotJ) = (for { i <- 0 until height; j <- 0 until width if map(i)(j) == '@' } yield (i, j)).head

    def move(command: Char): Unit = {
      def moveToDirection(iChange: Int, jChange: Int): Unit = {
        def moveHead(): Unit = {
          map(robotI)(robotJ) = '.'
          map(robotI + iChange)(robotJ + jChange) = '@'
          robotI += iChange
          robotJ += jChange
        }

        if (map(robotI + iChange)(robotJ + jChange) == '.') moveHead()
        else if (map(robotI + iChange)(robotJ + jChange) == 'O') {
          val nextI = (
            if (iChange > 0) robotI + 1 until height
            else if (iChange < 0) robotI - 1 to 0 by -1
            else List(robotI)
            ).find { map(_)(robotJ) != 'O' }.get

          val nextJ = (
            if (jChange > 0) robotJ + 1 until width
            else if (jChange < 0) robotJ - 1 to 0 by -1
            else List(robotJ)
            ).find { map(robotI)(_) != 'O' }.get

          map(nextI)(nextJ) match {
            case '.' => map(nextI)(nextJ) = 'O'; moveHead()
            case _   => ()
          }
        }
      }

      command match {
        case '>' => moveToDirection(0, 1)
        case '<' => moveToDirection(0, -1)
        case 'v' => moveToDirection(1, 0)
        case '^' => moveToDirection(-1, 0)
      }
    }

    commands.foreach { c =>
      //      println(c)
      move(c)
      //      println(map.map(_.mkString).mkString("", "\n", "\n"))
    }

    (for { i <- 0 until height; j <- 0 until width if map(i)(j) == 'O' } yield 100 * i + j).sum
  }

  "Day #15 Challenge #2" should {
    "work as expected #1" in {
      moveBoxes("""########
                  |#..O.O.#
                  |##@.O..#
                  |#...O..#
                  |#.#.O..#
                  |#...O..#
                  |#......#
                  |########
                  |
                  |<^^>>>vv<v>>v<<""".stripMargin.split("\n")) shouldBe 2028
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
      ) shouldBe 10092
    }

    "work as expected #3" in {
      val input = Utils.readInputFile(15)
      moveBoxes(input.toArray) shouldBe 1487337
    }
  }
}
