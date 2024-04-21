package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_merge_sorted_array extends AnyWordSpec with Matchers {
  // println(s"i1: $i1, i2: $i2, rI: $rI, nums1: ${nums1.mkString(", ")}")

  def merge(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit = {
    import scala.annotation.tailrec

    @tailrec
    def loop(i1: Int, i2: Int, rI: Int): Unit = if (i2 >= 0)
      if (i1 >= 0 && nums1(i1) > nums2(i2)) {
        nums1(rI) = nums1(i1)
        loop(i1 - 1, i2, rI - 1)
      } else {
        nums1(rI) = nums2(i2)
        loop(i1, i2 - 1, rI - 1)
      }

    loop(m - 1, n - 1, n + m - 1)
  }

  "merge" should {
    "work as expected" in {
      val num1_1 = Array(1, 2, 3, 0, 0, 0)
      merge(num1_1, 3, Array(2, 5, 6), 3)
      num1_1.toList shouldBe List(1, 2, 2, 3, 5, 6)
      println()

      val num1_2 = Array(1)
      merge(num1_2, 1, Array.empty, 0)
      num1_2.toList shouldBe List(1)
      println()

      val num1_3 = Array(0)
      merge(num1_3, 0, Array(1), 1)
      num1_3.toList shouldBe List(1)
      println()

      val num1_4 = Array(2, 0)
      merge(num1_4, 1, Array(1), 1)
      num1_4.toList shouldBe List(1, 2)
      println()

      val num1_5 = Array.fill(4)(0)
      val num2_5 = Array(-5, -2, 1, 3)
      merge(num1_5, 0, num2_5, 4)
      num1_5.toList shouldBe num2_5.toList

      val num1_6 = Array.fill(63)(0)
      //@formatter:off
      val num2_6 = Array(
        -50, -50, -48, -47, -44, -44, -37, -35, -35, -32,
        -32, -31, -29, -29, -28, -26, -24, -23, -23, -21,
        -20, -19, -17, -15, -14, -12, -12, -11, -10, -9,
        -8,  -5,  -2,  -2,  1,   1,   3,   4,   4,   7,
        7,   7,   9,   10,  11,  12,  14,  16,  17,  18,
        21,  21,  24,  31,  33,  34,  35,  36,  41,  41,
        46,  48,  48)
      //@formatter:off
      merge(num1_6, 0, num2_6, 63)
      num1_6.toList shouldBe num2_6.toList
    }
  }
}
