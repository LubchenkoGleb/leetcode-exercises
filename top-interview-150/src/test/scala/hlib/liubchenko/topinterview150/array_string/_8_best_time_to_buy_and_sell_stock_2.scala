package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _8_best_time_to_buy_and_sell_stock_2 extends AnyWordSpec with Matchers {
  def maxProfit(prices: Array[Int]): Int = {
    var windowStart = 0
    var profitAcc = 0

    prices.indices.drop(1).foreach { i =>
      if(prices(i - 1) > prices(i)) {
        profitAcc += (prices(i - 1) - prices(windowStart))
        windowStart = i
      }
    }

    profitAcc + (prices.last - prices(windowStart))
  }

  "maxProfit" should {
    "work as expected" in {
      maxProfit(Array(7, 1, 5, 3, 6, 4)) shouldBe 7
      maxProfit(Array(7, 6, 4, 3, 1)) shouldBe 0
      maxProfit(Array(1, 2, 3, 4, 5)) shouldBe 4
    }
  }
}
