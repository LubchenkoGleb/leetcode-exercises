package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _16_trapping_rain_water extends AnyWordSpec with Matchers {
  def trap(height: Array[Int]): Int = ???

  "trap" should {
    "work as expected" in {
      pending

      trap(Array(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1)) shouldBe 6
      trap(Array(4, 2, 0, 3, 2, 5)) shouldBe 9
    }
  }
}