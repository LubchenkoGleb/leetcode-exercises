package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _7_best_time_to_buy_and_sell_stock extends AnyWordSpec with Matchers {
  def maxProfit_v1(prices: Array[Int]): Int =
    prices
      .foldLeft((Int.MaxValue, Int.MinValue, 0)) { case ((min, max, acc), v) =>
        val newMin = if (v < min) v else min
        val newMax = if (min > newMin) newMin else if (v > max) v else max
        val newAcc = if (acc > newMax - newMin) acc else newMax - newMin

        (newMin, newMax, newAcc)
      }
      ._3

  def maxProfit_v2(prices: Array[Int]): Int =
    prices
      .foldLeft((Int.MaxValue, 0)) { case ((min, acc), v) =>
        val newMin = if (v < min) v else min
        val newMax = if (min > newMin) newMin else if (v > min + acc) v else min + acc
        val newAcc = if (acc > newMax - newMin) acc else newMax - newMin

        (newMin, newAcc)
      }
      ._2

  def maxProfit(prices: Array[Int]): Int =
    prices
      .foldLeft((Int.MaxValue, 0)) { case ((min, acc), v) =>
        val newMin = if (v < min) v else min
        val newAcc = if (acc > v - newMin) acc else v - newMin

        (newMin, newAcc)
      }
      ._2

  "maxProfit" should {
    "work as expected" in {
      maxProfit(Array(7, 1, 5, 3, 6, 4)) shouldBe 5
      maxProfit(Array(7, 6, 4, 3, 1)) shouldBe 0
    }
  }
}
