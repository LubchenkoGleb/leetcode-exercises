package hlib.liubchenko.adventofcode._2024.day2

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge1 extends AnyWordSpec with Matchers {
  def findSafeReports(lines: List[String]): Int = {
    def validate(array: Array[Int]): Int = {
      val isIncrease = array(1) > array(0)

      val res = array.sliding(2).forall {
        case Array(a, b) if isIncrease => b - a > 0 && b - a <= 3
        case Array(a, b)               => a - b > 0 && a - b <= 3
      }

      if (res) 1 else 0
    }

    lines
      .map(_.split(" ").map(_.toInt))
      .map(validate)
      .sum
  }

  "Day #2 Challenge #1" should {
    "work as expected #1" in {
      findSafeReports(
        List(
          "7 6 4 2 1",
          "1 2 7 8 9",
          "9 7 6 2 1",
          "1 3 2 4 5",
          "8 6 4 4 1",
          "1 3 6 7 9"
        )
      ) shouldBe 2
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(2)
      findSafeReports(input) shouldBe 407
    }
  }
}
