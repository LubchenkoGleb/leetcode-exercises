package hlib.liubchenko.adventofcode._2024.day15

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge2 extends AnyWordSpec with Matchers {
  def method(lines: List[String]): Int = 0

  "Day #15 Challenge #2" should {
    "work as expected #1" in {
      method(List.empty) shouldBe 0
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(15)
      method(input) shouldBe 0
    }
  }
}
