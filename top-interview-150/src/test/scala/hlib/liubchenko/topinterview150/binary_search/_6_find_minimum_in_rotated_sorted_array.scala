package hlib.liubchenko.topinterview150.binary_search

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _6_find_minimum_in_rotated_sorted_array extends AnyWordSpec with Matchers {
  def findMin(nums: Array[Int]): Int = {
    import scala.annotation.tailrec

    val last = nums.last

    @tailrec
    def loop(from: Int, to: Int): Int =
      if (from == to) nums(from)
      else {
        val mid = from + (to - from) / 2
        if (nums(mid) < last) loop(from, mid)
        else loop(mid + 1, to)
      }

    loop(0, nums.length - 1)
  }

  "findMin" should {
    "work as expected" in {
      findMin(Array(4, 5, 6, 7, 0, 1, 2)) shouldBe 0
      findMin(Array(7, 0, 1, 2, 4, 5, 6)) shouldBe 0
      findMin(Array(11, 13, 15, 17)) shouldBe 11
    }
  }
}
