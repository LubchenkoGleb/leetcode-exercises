package hlib.liubchenko.leetcode75.bit_manipulation

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_counting_bits extends AnyWordSpec with Matchers {
  def countBits(n: Int): Array[Int] = {
    // (0 to n).map { i => Integer.toBinaryString(i).count(_ == '1') }.toArray
//    Array
//      .range(0, n + 1)
//      .map { i =>
//        var ones = 0
//        var copy = i
//        while (copy > 0) {
//          ones += copy & 1
//          copy >>= 1
//        }
//        ones
//      }
    Array.range(0, n + 1).map(Integer.bitCount)
  }

  "countBits" should {
    "work" in {
      countBits(5) shouldBe Array(0, 1, 1, 2, 1, 2)
    }
  }
}
