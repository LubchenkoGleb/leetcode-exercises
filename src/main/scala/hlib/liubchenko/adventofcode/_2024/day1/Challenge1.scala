package hlib.liubchenko.adventofcode._2024.day1

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class Challenge1 extends AnyWordSpec with Matchers {
  def findDistance(lines: List[String]): Int = {
    val (l1, l2) = lines.map(_.split("[ \\t]+")).map { case Array(a, b) => a.toInt -> b.toInt }.unzip
    l1.sorted.zip(l2.sorted).map { case (a, b) => math.abs(a - b) }.sum
  }

  "findDistance" should {
    "work as expected #1" in {
      findDistance(
        List(
          "3   4",
          "4   3",
          "2   5",
          "1   3",
          "3   9",
          "3   3"
        )
      ) shouldBe 11
    }

    "work as expected #2" in {
      val input = Utils.readInputFile("src/main/scala/hlib/liubchenko/adventofcode/_2024/day1/input.txt")
      findDistance(input) shouldBe 3714264
    }
  }
}
