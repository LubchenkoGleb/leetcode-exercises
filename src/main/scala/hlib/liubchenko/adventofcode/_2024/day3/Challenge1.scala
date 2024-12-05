package hlib.liubchenko.adventofcode._2024.day3

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class Challenge1 extends AnyWordSpec with Matchers {
  def fixMemory(lines: List[String]): Int = {
    def fixLine(str: String): Int = "mul\\((\\d{1,3},\\d{1,3})\\)".r
      .findAllIn(str)
      .matchData
      .map(_.group(1))
      .map(_.split(",").map(_.toInt).product)
      .sum

    lines
      .map(fixLine)
      .sum
  }

  "fixMemory" should {
    "work as expected #1" in {
      fixMemory(List("xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")) shouldBe 161
    }

    "work as expected #2" in {
      val input = Utils.readInputFile("src/main/scala/hlib/liubchenko/adventofcode/_2024/day3/input.txt")
      fixMemory(input) shouldBe 180233229
    }
  }
}
