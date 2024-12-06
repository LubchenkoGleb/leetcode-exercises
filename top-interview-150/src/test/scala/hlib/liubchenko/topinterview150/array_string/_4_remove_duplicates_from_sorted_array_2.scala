package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _4_remove_duplicates_from_sorted_array_2 extends AnyWordSpec with Matchers {
  def removeDuplicates(nums: Array[Int]): Int =
    if (nums.length < 3) nums.length
    else nums.indices.drop(2).foldLeft(2) { (allowedIndex, i) =>
        if (nums(i) != nums(allowedIndex - 1) || nums(i) != nums(allowedIndex - 2)) {
          nums(allowedIndex) = nums(i)
          allowedIndex + 1
        } else allowedIndex
      }

  "removeDuplicates" should {
    "work as expected" in {
      val input = Array(1, 1, 1, 1, 2, 2, 2, 3, 3, 4)
      removeDuplicates(input) shouldEqual 7
      input.toList.take(7) shouldEqual List(1, 1, 2, 2, 3, 3, 4)
    }
  }
}
