package hlib.liubchenko.topinterview150.sliding_window

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_minimum_size_subarray_sum extends AnyWordSpec with Matchers {
  // println(s"i: $i, s: $s, currSum: $currSum, minLen: $minLen")

  def minSubArrayLen_2(target: Int, nums: Array[Int]): Int = {
    var (currSum, currLen, minLen) = (0, 0, nums.length)
    nums.indices.foreach { i =>
      currSum += nums(i); currLen += 1;
      while ((currSum - nums(i - currLen + 1)) >= target) {
        currLen -= 1; currSum -= nums(i - currLen)
      }
      if (currSum >= target) minLen = math.min(minLen, currLen)
    }
    if (currSum < target) 0 else minLen
  }

  def minSubArrayLen(target: Int, nums: Array[Int]): Int = {
    var (start, currSum, minLen) = (0, 0, Int.MaxValue)
    nums.indices.foreach { i =>
      currSum += nums(i)
      while (currSum >= target) {
        minLen = math.min(minLen, i + 1 - start)
        currSum -= nums(start)
        start += 1
      }
    }
    if (minLen != Int.MaxValue) minLen else 0
  }

  "minSubArrayLen" should {
    "work as expected" in {
      minSubArrayLen(7, Array(2, 3, 1, 2, 4, 3)) shouldBe 2
      minSubArrayLen(4, Array(1, 4, 4)) shouldBe 1
      minSubArrayLen(11, Array(1, 1, 1, 1, 1, 1, 1, 1)) shouldBe 0
      minSubArrayLen(15, Array(1, 2, 3, 4, 5)) shouldBe 5
      minSubArrayLen(6, Array(10, 2, 3)) shouldBe 1
      minSubArrayLen(11, Array(1, 2, 3, 4, 5)) shouldBe 3
    }
  }
}
