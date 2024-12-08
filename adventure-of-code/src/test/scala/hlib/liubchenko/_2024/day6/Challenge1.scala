package hlib.liubchenko._2024.day6

import hlib.liubchenko._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.annotation.tailrec

class Challenge1 extends AnyWordSpec with Matchers {
  def findRoute(lines: List[String]): Int = {
    val map = lines.map(_.toArray).toArray

    @tailrec
    def search(i: Int, j: Int): Option[(Int, Int)] =
      if (i >= map.length) None
      else if (j >= map(i).length) search(i + 1, 0)
      else if (map(i)(j) == '^') Some((i, j))
      else search(i, j + 1)

    var (i, j) = search(0, 0).get
    var isExit = false

    def move(nextI: Int, nextJ: Int): Unit = {
      val direction = map(i)(j)

      if (map(nextI)(nextJ) != '#') {
        map(i)(j) = 'x'
        i = nextI; j = nextJ
        map(i)(j) = direction
      } else {
        map(i)(j) = direction match {
          case '^' => '>'
          case '>' => 'v'
          case '<' => '^'
          case 'v' => '<'
        }
      }
    }

    while (!isExit) {
      val (nextI, nextJ) = map(i)(j) match {
        case '^' => (i - 1, j)
        case '>' => (i, j + 1)
        case '<' => (i, j - 1)
        case 'v' => (i + 1, j)
      }
      if (nextI < 0 || nextI > map.length - 1 || nextJ < 0 || nextJ > map(0).length - 1) isExit = true
      else move(nextI, nextJ)
    }

    map.map(_.count(_ == 'x')).sum + 1
  }

  "Day #6 Challenge #1" should {
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
      ) shouldBe 41
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(6)
      findRoute(input) shouldBe 4819
    }
  }
}
