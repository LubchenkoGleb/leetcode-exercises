package hlib.liubchenko.topinterview150.bit_manipulation

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _5_single_number_2 extends AnyWordSpec with Matchers {
  def singleNumber(nums: Array[Int]): Int = {
    var (ones, twos) = (0, 0)

    for (n <- nums) { ones ^= n & ~twos; twos ^= n & ~ones }

    ones
  }

  "singleNumber" should {
    "work as expected" in {
      singleNumber(Array(0, 1, 0, 1, 0, 1, 99)) shouldBe 99
      singleNumber(Array(2, 2, 3, 2)) shouldBe 3
      singleNumber(Array(1, 2, 2, 2, 3, 3, 3)) shouldBe 1
    }
  }
}
