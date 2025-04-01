package hlib.liubchenko.topinterview150.multi_dimensional_dp

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _7_best_time_to_buy_and_sell_stock_3 extends AnyWordSpec with Matchers {
  def maxProfit(prices: Array[Int]): Int = { 0 }

  "maxProfit" should {
    "work as expected" in {
      maxProfit(Array(3, 3, 5, 0, 0, 3, 1, 4)) shouldBe 6
      maxProfit(Array(1, 2, 3, 4, 5)) shouldBe 4
      maxProfit(Array(7, 6, 4, 3, 1)) shouldBe 3
    }
  }
}
