package hlib.liubchenko.topinterview150.kadanes_algorithm

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_maximum_subarray extends AnyWordSpec with Matchers {
  def maxSubArray(nums: Array[Int]): Int = {
    var max = nums.head
    var acc = nums.head

    nums.tail.foreach { v =>
      acc = if (acc + v > v) acc + v else v
      max = math.max(max, acc)
    }

    max
  }

  "maxSubArray" should {
    "work as expected" in {
      maxSubArray(Array(-2, 1, -3, 4, -1, 2, 1, -5, 4)) shouldBe 6
      maxSubArray(Array(1)) shouldBe 1
      maxSubArray(Array(5, 4, -1, 7, 8)) shouldBe 23
      maxSubArray(Array(-1)) shouldBe -1
    }
  }
}
