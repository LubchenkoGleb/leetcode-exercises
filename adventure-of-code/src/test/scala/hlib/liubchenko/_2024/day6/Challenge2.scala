package hlib.liubchenko._2024.day6

import hlib.liubchenko._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.annotation.tailrec

class Challenge2 extends AnyWordSpec with Matchers {
  def findRoute(lines: List[String]): Int = {
    val map = lines.map(_.toArray).toArray

    @tailrec
    def search(i: Int, j: Int): Option[(Int, Int)] =
      if (i >= map.length) None
      else if (j >= map(i).length) search(i + 1, 0)
      else if (map(i)(j) == '^') Some((i, j))
      else search(i, j + 1)

    val (initialI, initialJ) = search(0, 0).get
    val initialDirection = map(initialI)(initialJ)

    val iterator = for {
      i <- map.indices
      j <- map.head.indices
      if map(i)(j) == '.'
    } yield (i, j)

    iterator.count { case (i, j) =>
      map(i)(j) = '0'
      checkLoop(initialI, initialJ, initialDirection, map)
    }
  }

  def checkLoop(initialI: Int, initialJ: Int, initialDirection: Char, map: Array[Array[Char]]): Boolean = {
    var (i, j, direction) = (initialI, initialJ, initialDirection)
    var isExit = false
    val visited = collection.mutable.Set.empty[(Int, Int, Char)]

    def resetMap(): Unit = for {
      i <- map.indices
      j <- map.head.indices
      if map(i)(j) != '#'
    } map(i)(j) = '.'

    while (!isExit && !visited.contains(i, j, direction)) {
      val (nextI, nextJ) = direction match {
        case '^' => (i - 1, j)
        case '>' => (i, j + 1)
        case '<' => (i, j - 1)
        case 'v' => (i + 1, j)
      }

      if (nextI < 0 || nextI > map.length - 1 || nextJ < 0 || nextJ > map(0).length - 1) isExit = true
      else {
        visited.add((i, j, direction))
        map(nextI)(nextJ) match {
          case '#' | '0' =>
            direction = direction match {
              case '^' => '>'
              case '>' => 'v'
              case '<' => '^'
              case 'v' => '<'
            }
          case _ =>
            i = nextI; j = nextJ
        }
      }
    }

    resetMap()

    visited.contains(i, j, direction)
  }

  "Day #6 Challenge #2 findRoute" should {
    "work as expected #1" in {
      findRoute(
        List(
          "....#.....",
          ".........#",
          "..........",
          "..#.......",
          ".......#..",
          "..........",
          ".#..^.....",
          "........#.",
          "#.........",
          "......#..."
        )
      ) shouldBe 6
    }

    "checkLoop #1" in {
      val map = List(
        "....#.....",
        ".........#",
        "..........",
        "..#.......",
        ".......#..",
        "..........",
        ".#..^.....",
        "........#.",
        "#0........",
        "......#..."
      ).map(_.toArray).toArray
      checkLoop(6, 4, '^', map) shouldBe true
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(6)
      findRoute(input) shouldBe 1796
    }
  }
}
