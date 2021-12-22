package hlib.liubchenko

object _37_bestTimeToBuyAndSellStock1 extends App {
  def maxProfit(prices: Array[Int]): Int = {
    if (prices.length < 2) return 0

    val (min, max, profit) = prices.tail
      .foldLeft((prices.head, prices.head, 0)) { case (acc @ (min, max, profit), next) =>
        if (next < min && profit < (max - min)) (next, next, max - min)
        else if (next < min) (next, next, profit)
        else if (next > max) (min, next, profit)
        else acc
      }

    math.max(max - min, profit)
  }

  def maxProfit2(prices: Array[Int]): Int = {
    if (prices.length < 2) return 0

    prices
      .foldLeft((Int.MaxValue, 0)) { case ((minPrice, maxProfit), next) =>
        if (next < minPrice) (next, maxProfit)
        else if (next - minPrice > maxProfit) (minPrice, next - minPrice)
        else (minPrice, maxProfit)
      }
      ._2
  }

  println(maxProfit2(Array(5, 1, 4, 2, 6))) // 5, hold
  println(maxProfit2(Array(5, 2, 6, 1, 4))) // 4, min not included in deal
  println(maxProfit2(Array(5, 4, 6, 1, 4))) // 3, max not included in deal
}
