package hlib.liubchenko.topinterview150.kadanes_algorithm

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_maximum_sum_circular_subarray extends AnyWordSpec with Matchers {
  def maxSubarraySumCircular(nums: Array[Int]): Int = {
    // https://leetcode.com/problems/maximum-subarray/solutions/1822821/scala-one-liner-functional-way-kadane-s-algorithm-with-explanation/?envType=study-plan-v2&envId=top-interview-150
    var max = nums.head
    var acc = nums.head
    var accI = 1
    var i = 1

    while (accI < nums.length) {
      if (i >= nums.length) i = 0

      val v = nums(i)
      acc = math.max(acc + v, v)
      max = math.max(acc, max)

      i += 1
      if (acc == v) accI = 1 else accI += 1
    }

    max
  }

  "maxSubArray" should {
    "work as expected" in {
      maxSubarraySumCircular(Array(-2, 1, -3, 4, -1, 2, 1, -5, 4)) shouldBe 6
      maxSubarraySumCircular(Array(1)) shouldBe 1
      maxSubarraySumCircular(Array(5, 4, -1, 7, 8)) shouldBe 23
      maxSubarraySumCircular(Array(-1)) shouldBe -1
      maxSubarraySumCircular(Array(5, -3, 5)) shouldBe 10
      maxSubarraySumCircular(Array(5, -10, 8, -10, 3, -1, 5)) shouldBe 13
    }
  }
}
