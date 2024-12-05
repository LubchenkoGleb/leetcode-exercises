package hlib.liubchenko.adventofcode._2024.day3

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class Challenge2 extends AnyWordSpec with Matchers {
  def fixMemory(str: String): Int = """do\(\)|don't\(\)|mul\((\d{1,3},\d{1,3})\)""".r
    .findAllIn(str)
    .matchData
    .foldLeft((true, 0)) { case ((switcher, sumAcc), m) =>
      if (m.toString().startsWith("mul") && switcher)
        (switcher, sumAcc + m.group(1).split(",").map(_.toInt).product)
      else if (m.toString() == "do()") (true, sumAcc)
      else if (m.toString() == "don't()") (false, sumAcc)
      else (switcher, sumAcc)
    }
    ._2

  "fixMemory" should {
    "work as expected #1" in {
      fixMemory("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))") shouldBe 48
    }

    "work as expected #2" in {
      val source = Source.fromFile("src/main/scala/hlib/liubchenko/adventofcode/_2024/day3/input.txt")
      val input = source.mkString
      source.close()

      fixMemory(input) shouldBe 95411583
    }
  }
}
