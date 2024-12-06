package hlib.liubchenko.topinterview150.binary_search

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_search_insert_position extends AnyWordSpec with Matchers {
  def searchInsert(nums: Array[Int], target: Int): Int = {
    import scala.annotation.tailrec

    @tailrec
    def loop(from: Int, to: Int): Int =
      if (from == to) {
        val value = nums(from)
        if (value >= target) from
        else from + 1
      } else {
        val half = from + ((to - from) / 2)
        if (target <= nums(half)) loop(from, half)
        else loop(half + 1, to)
      }

    loop(0, nums.length - 1)
  }

  "searchInsert" should {
    "work as expected" in {
      searchInsert(Array(1, 3, 5, 6), 5) shouldBe 2
      searchInsert(Array(1, 3, 5, 6, 7), 5) shouldBe 2
      searchInsert(Array(1, 3, 5, 6), 2) shouldBe 1
      searchInsert(Array(1, 3, 5, 6), 7) shouldBe 4
    }
  }
}
