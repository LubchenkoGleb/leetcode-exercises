package hlib.liubchenko.adventofcode._2024.day10

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge2 extends AnyWordSpec with Matchers {
  def findTrails(lines: List[String]): Int = {
    val map = lines.map(_.map(_.asDigit)).toArray

    def loop(i: Int, j: Int, prev: Int): Int = {
      if (i < 0 || i >= map.length || j < 0 || j >= map.head.length || map(i)(j) != prev + 1) 0
      else if (map(i)(j) == 9) 1
      else loop(i - 1, j, prev + 1) + loop(i + 1, j, prev + 1) + loop(i, j - 1, prev + 1) + loop(i, j + 1, prev + 1)
    }

    val routes = for {
      i <- map.indices
      j <- map.head.indices
      if map(i)(j) == 0
    } yield loop(i, j, -1)

    routes.sum
  }

  "Day #10 Challenge #2" should {
    "work as expected #1" in {
      findTrails(
        List(
          "3330333",
          "3331333",
          "3342433",
          "6543456",
          "7333337",
          "8333338",
          "9333339"
        )
      ) shouldBe 2
    }

    "work as expected #2" in {
      findTrails(
        List(
          "89010123",
          "78121874",
          "87430965",
          "96549874",
          "45678903",
          "32019012",
          "01329801",
          "10456732"
        )
      ) shouldBe 81
    }

    "work as expected #3" in {
      val input = Utils.readInputFile(10)
      findTrails(input) shouldBe 1380
    }
  }
}
