package leetcode_patterns

object _29_bestTimeToBuyAndSellStock2 extends App {
  def maxProfit(prices: Array[Int]): Int = {
    if (prices.length < 2) return 0

    prices.sliding(2).foldLeft(0) { case (acc, Array(prev, next)) =>
      if (next > prev) acc + next - prev else acc
    }
  }

  println(maxProfit(Array(7, 1, 5, 3, 6, 4)))
  println(maxProfit(Array(1, 2, 3, 4, 5)))
  println(maxProfit(Array(7, 6, 4, 3, 1)))
}
