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

    var direction = search(0, 0).get

    0
  }

  "Day #6 Challenge #1 findRoute" should {
    "work as expected #1" in {
      findRoute(
        List(
          "....#.....",
          "....^....#",
          "..........",
          "..#.......",
          ".......#..",
          "..........",
          ".#........",
          "........#.",
          "#.........",
          "......#..."
        )
      ) shouldBe 0
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(6)
      findRoute(input) shouldBe 0
    }
  }
}
