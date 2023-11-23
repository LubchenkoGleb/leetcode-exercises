package hlib.liubchenko.leetcode75.prefix_sum

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_find_pivot_index extends AnyWordSpec with Matchers {
  import scala.annotation.tailrec

  def pivotIndex(nums: Array[Int]): Int = {
    @tailrec
    def loop(i: Int, lSum: Int, rSum: Int): Int =
      if (rSum == lSum) i
      else if (i == nums.length - 1) if (lSum == 0) nums.length - 1 else -1
      else loop(i + 1, lSum + nums(i), rSum - nums(i + 1))

    val initialRSum = (1 until nums.length).map(nums).sum
    loop(0, 0, initialRSum)
  }

  "pivotIndex" should {
    "work as expected" in {
      pivotIndex(Array(1, 7, 3, 6, 5, 6)) shouldBe 3
      pivotIndex(Array(1, 2, 3)) shouldBe -1
      pivotIndex(Array(-1, 1, 3)) shouldBe 2
    }
  }
}
