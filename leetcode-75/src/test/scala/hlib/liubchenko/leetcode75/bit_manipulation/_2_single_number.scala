package hlib.liubchenko.leetcode75.bit_manipulation

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class _2_single_number extends AnyWordSpec with Matchers {
  def singleNumber(nums: Array[Int]): Int = nums.fold(0)(_ ^ _)

  def singleNumber_2(nums: Array[Int]): Int =
    nums.sortInPlace
      .sliding(2, 2)
      .collectFirst {
        case mutable.ArraySeq(a, b) if a != b => a
        case mutable.ArraySeq(a)              => a
      }
      .get

  "singleNumber" should {
    "work as expected" in {
      singleNumber(Array(2, 2, 1)) shouldBe 1
      singleNumber(Array(4, 1, 2, 1, 2)) shouldBe 4
      singleNumber(Array(1)) shouldBe 1
    }
  }
}
