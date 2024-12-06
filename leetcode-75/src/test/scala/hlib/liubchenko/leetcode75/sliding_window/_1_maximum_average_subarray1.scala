package hlib.liubchenko.leetcode75.sliding_window

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

// 2023/11/07 Need Repeat
class _1_maximum_average_subarray1 extends AnyWordSpec with Matchers {

  // My refactor of findMaxAverage_4
  // Runtime 54, Memory 59
  def findMaxAverage(nums: Array[Int], k: Int): Double = {
    val sum, maxSum = nums.take(k).sum
    (k until nums.length)
      .foldLeft(sum -> maxSum) { case ((sum, maxSum), i) =>
        val updateSumAcc = sum + nums(i) - nums(i - k)
        updateSumAcc -> math.max(maxSum, updateSumAcc)
      }
      ._2
      .toDouble / k
  }

  // Not mine
  // Runtime 100, memory 100
  def findMaxAverage_4(nums: Array[Int], k: Int): Double = {
    var sum, maxSum = nums.take(k).sum
    for (i <- k until nums.length) {
      sum += nums(i) - nums(i - k)
      maxSum = math.max(maxSum, sum)
    }
    maxSum.toDouble / k.toDouble
  }

  // 124/127
  // memory limit exceeded
  def findMaxAverage_3(nums: Array[Int], k: Int): Double = {
    //    (0 to nums.length - k)
    //      .foldLeft(nums.slice(0, k).sum.toDouble / k) { case (avgAcc, i) =>
    //        val newAvg = (0 until k).foldLeft(0) { case (summAcc, j) => summAcc + nums(i + j) }.toDouble / k
    //        math.max(avgAcc, newAvg)
    //      }

    Iterator
      .range(0, nums.length - k + 1)
      .foldLeft(nums.slice(0, k).sum.toDouble / k) { case (avgAcc, i) =>
        val newAvg = Iterator.range(0, k).foldLeft(0) { case (summAcc, j) => summAcc + nums(i + j) }.toDouble / k
        math.max(avgAcc, newAvg)
      }
  }

  // complexity n * k
  // memory limit exceeded
  def findMaxAverage_2(nums: Array[Int], k: Int): Double = {
    nums.sliding(k).foldLeft(Double.MinValue) { case (acc, arr) => math.max(arr.sum.toDouble / k, acc) }
  }

  // complexity n * k
  // memory limit exceeded
  def findMaxAverage_1(nums: Array[Int], k: Int): Double = {
    nums.sliding(k).map(_.sum.toDouble / k).max
  }

  "findMaxAverage" should {
    "work as expected" in {
//      findMaxAverage(Array(1, 12, -5, -6, 50, 3), 4) shouldBe 12.75
//      findMaxAverage(Array(0, 1, 1, 3, 3), 4) shouldBe 2.0
      findMaxAverage(Array(0, 4, 0, 3, 2), 1) shouldBe 4.0
    }
  }
}
