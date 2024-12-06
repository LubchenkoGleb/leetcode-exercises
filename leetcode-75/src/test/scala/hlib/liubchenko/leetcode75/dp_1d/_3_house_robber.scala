package hlib.liubchenko.leetcode75.dp_1d

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_house_robber extends AnyWordSpec with Matchers {
  import scala.annotation.tailrec

  // My, Runtime 82, Memory 40
  def rob(nums: Array[Int]): Int = {
    @tailrec
    def loop(iM2Acc: Int, iM1Acc: Int, i: Int): Int =
      if (i == nums.length) math.max(iM2Acc, iM1Acc)
      else loop(iM1Acc, math.max(iM1Acc, iM2Acc + nums(i)), i + 1)

    loop(0, 0, 0)
  }

  // My, Runtime 20, Memory 73
  def rob_2(nums: Array[Int]): Int = {

    @tailrec
    def loop(iM3Acc: Int, iM2Acc: Int, iM1Acc: Int, i: Int): Int =
      if (i == nums.length) Array(iM3Acc, iM2Acc, iM1Acc).max
      else loop(iM2Acc, iM1Acc, Array(iM3Acc + nums(i), iM2Acc + nums(i), iM1Acc).max, i + 1)

    loop(0, 0, 0, 0)
  }

  "rob" should {
    "work as expected" in {
//      rob(Array(1, 2, 3, 1)) shouldBe 4
//      rob(Array(2, 7, 9, 3, 1)) shouldBe 12
      rob(Array(2, 1, 1, 2)) shouldBe 4
    }
  }
}
