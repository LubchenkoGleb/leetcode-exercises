package hlib.liubchenko.topinterview150.binary_search

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _5_find_first_and_last_position_of_element_in_sorted_array extends AnyWordSpec with Matchers {
  def searchRange(nums: Array[Int], target: Int): Array[Int] = {
    def loop(i: Int, j: Int): Array[Int] =
      if (i > j) Array(-1, -1)
      else if (i == j && nums(i) == target) Array(i, i)
      else {
        val mid = i + (j - i) / 2
        if (target < nums(mid)) loop(i, mid - 1)
        else if (nums(mid) < target) loop(mid + 1, j)
        else {
          val l = loop(i, mid)
          val r = loop(mid + 1, j)
          Array(l(0), math.max(l(1), r(1)))
        }
      }

    loop(0, nums.length - 1)
  }

  "searchRange" should {
    "work as expected" in {
      searchRange(nums = Array(5, 7, 7, 8, 8, 10), target = 8) shouldBe Array(3, 4)
      searchRange(nums = Array(5, 7, 7, 8, 8, 10), target = 6) shouldBe Array(-1, -1)
      searchRange(nums = Array(), target = 0) shouldBe Array(-1, -1)
    }
  }

}
