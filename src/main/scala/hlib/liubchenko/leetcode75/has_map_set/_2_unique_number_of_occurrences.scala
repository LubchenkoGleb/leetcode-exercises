package hlib.liubchenko.leetcode75.has_map_set

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_unique_number_of_occurrences extends AnyWordSpec with Matchers {
  def uniqueOccurrences(arr: Array[Int]): Boolean = {
    val count = arr.groupMapReduce(n => n)(_ => 1)(_ + _)
    count.size == count.values.toSet.size
  }

  "uniqueOccurrences" should {
    "work as expected" in {
      uniqueOccurrences(Array(1, 2, 2, 1, 1, 3)) shouldBe true
    }
  }
}
