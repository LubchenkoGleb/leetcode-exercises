package hlib.liubchenko.leetcode75.dp_1d

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_min_cost_climbing_stairs extends AnyWordSpec with Matchers {
  // My, Runtime 82, Memory 60
  def minCostClimbingStairs(cost: Array[Int]): Int = {
    import scala.annotation.tailrec

    @tailrec
    def loop(iM2Acc: Int, iM1Acc: Int, i: Int): Int =
      if (i == cost.length) math.min(iM1Acc, iM2Acc)
      else loop(iM1Acc, math.min(iM2Acc + cost(i), iM1Acc + cost(i)), i + 1)

    loop(cost(0), cost(1), 2)
  }

  "minCostClimbingStairs" should {
    "work as expected" in {
      minCostClimbingStairs(Array(10, 15, 20)) shouldBe 15
      minCostClimbingStairs(Array(1, 100, 1, 1, 1, 100, 1, 1, 100, 1)) shouldBe 6
    }
  }
}
