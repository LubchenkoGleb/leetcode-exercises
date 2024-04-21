package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.wordspec.AnyWordSpec

class _3_remove_duplicates_from_sorted_Array extends AnyWordSpec with Matchers {
  def removeDuplicates(nums: Array[Int]): Int = {
    import scala.annotation.tailrec

    @tailrec
    def loop(lastUniqueIndex: Int, index: Int): Int =
      if (index == nums.length) lastUniqueIndex + 1
      else if (nums(lastUniqueIndex) != nums(index)) {
        nums(lastUniqueIndex + 1) = nums(index)
        loop(lastUniqueIndex + 1, index + 1)
      } else loop(lastUniqueIndex, index + 1)

    if (nums.isEmpty) 0
    else loop(0, 1)
  }

  "removeDuplicates" should {
    "work as expected" in {
      val table = Table(
        ("input", "expectedNumber", "expectedArray"),
        (Array(1, 1, 2), 2, Array(1, 2, 2)),
        (Array.empty[Int], 0, Array.empty[Int]),
        (Array(0, 0, 1, 1, 1, 2, 2, 3, 3, 4), 5, Array(0, 1, 2, 3, 4, 2, 2, 3, 3, 4))
      )

      forAll(table) { (input, expectedUniqueCount, expectedArray) =>
        removeDuplicates(input) shouldBe expectedUniqueCount
        input.toList shouldBe expectedArray.toList
      }
    }
  }
}
