package hlib.liubchenko.topinterview150.multi_dimensional_db

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _8_best_time_to_buy_and_sell_stock_4 extends AnyWordSpec with Matchers {
  def maxProfit(k: Int, prices: Array[Int]): Int = {
    def loop(remDeals: Int, curr: Int, i: Int): Int = {
      println(s"remDeals: $remDeals, curr: $curr, i: $i")
      if (remDeals == 0 || curr >= prices.length) {
        println("if")
        0
      } else if (i >= prices.length) {
        println("else if")
        prices.last - prices(curr)
      } else {
        println("else")
        val deal = prices(i) - prices(curr)
        println(s"deal: $deal")
        val r1 = deal + loop(remDeals - 1, i + 1, i + 2)
        println(s"r1: $r1")
        val r2 = loop(remDeals, curr, i + 1)
        println(s"r1: $r1, r2: $r2")
        val r3 = loop(remDeals, curr + 1, curr + 2)
        List(r1, r2, r3).max
      }
    }

    loop(k, 0, 1)
  }

  "maxProfit" should {
    "work as expected" in {
//      maxProfit(k = 2, prices = Array(2, 4, 1)) shouldBe 2
      maxProfit(k = 2, prices = Array(3, 2, 6, 5, 0, 3)) shouldBe 7
    }
  }
}
