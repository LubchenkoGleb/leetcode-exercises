package hlib.liubchenko.adventofcode._2024.day16

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge2 extends AnyWordSpec with Matchers {
  def method(lines: List[String]): Int = 0

  "Day #16 Challenge #2" should {
    "work as expected #1" in {
      method(List.empty) shouldBe 0
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(16)
      method(input) shouldBe 0
    }
  }
}
