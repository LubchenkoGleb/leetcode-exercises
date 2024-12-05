package hlib.liubchenko.adventofcode._2024.day2

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class Challenge2 extends AnyWordSpec with Matchers {
  def findSafeReports(lines: List[String]): Int = {
    def validate(array: Array[Int]): Boolean = {
      val isIncrease = array(1) > array(0)

      array.sliding(2).forall {
        case Array(a, b) if isIncrease => b - a > 0 && b - a <= 3
        case Array(a, b)               => a - b > 0 && a - b <= 3
      }
    }

    lines
      .map(_.split(" ").map(_.toInt))
      .map { array =>
        val res = array.indices.exists { indexToSkip =>
          val filteredArray = array.zipWithIndex.collect { case (v, i) if i != indexToSkip => v }
          validate(filteredArray)
        }
        if (res) 1 else 0
      }
      .sum
  }

  "findSafeReports" should {
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
      ) shouldBe 4
    }

    "work as expected #2" in {
      val input = Utils.readInputFile("src/main/scala/hlib/liubchenko/adventofcode/_2024/day2/input.txt")
      findSafeReports(input) shouldBe 459
    }
  }
}
