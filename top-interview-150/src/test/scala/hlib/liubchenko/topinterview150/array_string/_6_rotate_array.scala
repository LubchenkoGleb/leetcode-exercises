package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _6_rotate_array extends AnyWordSpec with Matchers {
  def rotate(nums: Array[Int], k: Int): Unit = if (k != 0 && k % nums.length != 0) {
    val effectiveK = k % nums.length
    reverse(nums, 0, nums.length - 1)
    reverse(nums, 0, effectiveK - 1)
    reverse(nums, effectiveK, nums.length - 1)
  }

  def reverse(nums: Array[Int], from: Int, to: Int): Unit =
    (0 to (to - from) / 2).foreach { i =>
      val tmp = nums(from + i)
      nums(from + i) = nums(to - i)
      nums(to - i) = tmp
    }

  "rotate" should {
    "work as expected" in {
      var input = Array(1, 2, 3, 4, 5, 6, 7)
      rotate(input, 3)
      input.toList shouldBe List(5, 6, 7, 1, 2, 3, 4)

      input = Array(-1, -100, 3, 99)
      rotate(input, 2)
      input.toList shouldBe List(3, 99, -1, -100)

      input = Array(1)
      rotate(input, 0)
      input.toList shouldBe List(1)

      input = Array(1)
      rotate(input, 1)
      input.toList shouldBe List(1)

      input = Array(-1)
      rotate(input, 2)
      input.toList shouldBe List(-1)
    }
  }
}

// 1 2 3 4 5 6 7
// 4 2
