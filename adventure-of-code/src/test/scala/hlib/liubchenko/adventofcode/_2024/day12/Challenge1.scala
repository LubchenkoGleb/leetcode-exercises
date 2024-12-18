package hlib.liubchenko.adventofcode._2024.day12

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge1 extends AnyWordSpec with Matchers {
  def fencePrice(lines: List[String]): Int = 0

  "Day #12 Challenge #1" should {
    "work as expected #1" in {
      val input = List(
        "AAAA",
        "BBCD",
        "BBCC",
        "EEEC"
      )
      fencePrice(input) shouldBe 36
    }

    "work as expected #2" in {
      val input = List(
        "OOOOO",
        "OXOXO",
        "OOOOO",
        "OXOXO",
        "OOOOO"
      )
      fencePrice(input) shouldBe 140
    }

    "work as expected #3" in {
      val input = List(
        "RRRRIICCFF",
        "RRRRIICCCF",
        "VVRRRCCFFF",
        "VVRCCCJFFF",
        "VVVVCJJCFE",
        "VVIVCCJJEE",
        "VVIIICJJEE",
        "MIIIIIJJEE",
        "MIIISIJEEE",
        "MMMISSJEEE"
      )
      fencePrice(input) shouldBe 1930
    }

    "work as expected #4" in {
      val input = Utils.readInputFile(12)
      fencePrice(input) shouldBe 0
    }
  }
}
