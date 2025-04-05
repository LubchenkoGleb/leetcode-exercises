package hlib.liubchenko.topinterview150.binary_search

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _4_search_in_rotated_sorted_array extends AnyWordSpec with Matchers {
  def search(nums: Array[Int], target: Int): Int = {
    import scala.annotation.tailrec

    @tailrec
    def shift(i: Int, j: Int)(mid: => Int = i + (j - i) / 2): Int =
      if (nums(i) < nums(j)) i
      else if (i + 1 >= j) j
      else if (nums(i) <= nums(mid)) shift(mid, j)()
      else shift(i, mid)()

    val k = shift(0, nums.length - 1)()
    def n(i: Int): Int = if (i + k >= nums.length) i + k - nums.length else i + k

    @tailrec
    def loop(i: Int, j: Int)(mid: => Int = i + (j - i) / 2): Int =
      if (i == j) if (nums(n(i)) == target) n(i) else -1
      else if (target <= nums(n(mid))) loop(i, mid)()
      else loop(mid + 1, j)()

    loop(0, nums.length - 1)()
  }

  "search" should {
    "work as expected" in {
      search(nums = Array(0, 1, 2, 4, 5, 6, 7), target = 3) shouldBe -1
      search(nums = Array(0, 1, 2, 4, 5, 6, 7), target = 6) shouldBe 5
      search(nums = Array(4, 5, 6, 7, 0, 1, 2), target = 0) shouldBe 4
      search(nums = Array(6, 7, 0, 1, 2, 4, 5), target = 0) shouldBe 2
      search(nums = Array(4, 5, 6, 7, 0, 1, 2), target = 3) shouldBe -1
      search(nums = Array(1), target = 0) shouldBe -1
      search(nums = Array(1, 3), target = 1) shouldBe 0
    }
  }
}
