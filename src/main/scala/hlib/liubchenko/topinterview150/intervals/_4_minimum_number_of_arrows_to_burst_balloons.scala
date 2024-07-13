package hlib.liubchenko.topinterview150.intervals

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _4_minimum_number_of_arrows_to_burst_balloons extends AnyWordSpec with Matchers {
  def findMinArrowShots(points: Array[Array[Int]]): Int = {
    0
  }

  "findMinArrowShots" should {
    "work as expected" in {
      findMinArrowShots(Array(Array(10, 16), Array(2, 8), Array(1, 6), Array(7, 12))) shouldBe 2
      findMinArrowShots(Array(Array(1, 2), Array(3, 4), Array(5, 6), Array(7, 8))) shouldBe 4
      findMinArrowShots(Array(Array(1, 2), Array(2, 3), Array(3, 4), Array(4, 5))) shouldBe 2
    }
  }
}
