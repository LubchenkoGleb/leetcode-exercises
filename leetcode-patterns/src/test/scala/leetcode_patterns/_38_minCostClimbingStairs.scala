package leetcode_patterns

object _38_minCostClimbingStairs extends App {
  def minCostClimbingStairs(cost: Array[Int]): Int = {
    def loop(i: Int, costAcc: Int): Int = {
      val newAcc = if (i < 2) costAcc + cost(i) else costAcc

      if (i >= cost.length - 2) newAcc
      else math.min(loop(i + 1, newAcc + cost(i + 1)), loop(i + 2, newAcc + cost(i + 2)))
    }

    loop(0, 0)
  }

  def minCostClimbingStairs2(cost: Array[Int]): Int = {
    val n = cost.length
    val acc = new Array[Int](n + 1)
    (2 to n).foreach { i =>
      acc(i) = math.min(acc(i - 2) + cost(i - 2), acc(i - 1) + cost(i - 1))
    }

    acc(n)
  }

  println(minCostClimbingStairs2(Array(1, 100, 1, 1, 1, 100, 1, 1, 100, 1))) // l = 10, a = 6
  println(minCostClimbingStairs2(Array(1, 50, 100, 1, 100, 1, 1, 1, 100, 1))) // l = 10, a = 54
}
