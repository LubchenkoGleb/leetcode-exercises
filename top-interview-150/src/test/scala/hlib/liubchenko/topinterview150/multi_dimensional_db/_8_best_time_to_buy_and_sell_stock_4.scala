package hlib.liubchenko.topinterview150.multi_dimensional_db

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _8_best_time_to_buy_and_sell_stock_4 extends AnyWordSpec with Matchers {
  def maxProfit(k: Int, prices: Array[Int]): Int = {

    0
  }

  "maxProfit" should {
    "work as expected" in {
      maxProfit(k = 2, prices = Array(2, 4, 1)) shouldBe 2
      maxProfit(k = 2, prices = Array(3, 2, 6, 5, 0, 3)) shouldBe 7
    }
  }
}
