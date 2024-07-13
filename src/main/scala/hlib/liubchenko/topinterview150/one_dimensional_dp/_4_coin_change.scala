package hlib.liubchenko.topinterview150.one_dimensional_dp

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _4_coin_change extends AnyWordSpec with Matchers {
  def coinChange(coins: Array[Int], amount: Int): Int = {
    val dp = Array.fill(amount + 1)(Int.MaxValue)
    dp(0) = 0

    for {
      coin <- coins
      x <- coin to amount
    } {
      var diff = dp(x - coin)
      if (diff != Int.MaxValue) diff += 1
      dp(x) = math.min(dp(x), diff)
    }

    if (dp(amount) == Int.MaxValue) -1 else dp(amount)
  }

  // Memory Limit
  def coinChange_2(coins: Array[Int], amount: Int): Int = {
    val coinsList = coins.sortInPlace()(Ordering[Int].reverse).toList

    def loop2(remaining: Int, coins: List[Int]): Int =
      if (remaining % coins.head == 0) remaining / coins.head
      else if (coins.size == 1) Int.MaxValue
      else
        (0 to remaining / coins.head)
          .map { biggestCoinTimes =>
            val coinsToAdd = loop2(remaining - biggestCoinTimes * coins.head, coins.tail)
            if (coinsToAdd == Int.MaxValue) coinsToAdd
            else {
              val res = biggestCoinTimes + coinsToAdd
              if (remaining == 332) println(s"res: $res, coin: ${coins.head}, times: $biggestCoinTimes")
              res
            }
          }
          .minOption
          .getOrElse(Int.MaxValue)

    val res = loop2(amount, coinsList)
    if (res == Int.MaxValue) -1 else res
  }

  "coinChange" should {
    "work as expected" in {
      coinChange(Array(1, 2, 5), 11) shouldBe 3
      coinChange(Array(2, 4, 6), 11) shouldBe -1
      coinChange(Array(1), 0) shouldBe 0
      coinChange(Array(186, 419, 83, 408), 6249) shouldBe 20 // 419:5, 408:8, 186:3, 83:4
      coinChange(Array(411, 412, 413, 414, 415, 416, 417, 418, 419, 420, 421, 422), 9864) shouldBe 24
      coinChange(Array(2, 5), 8) shouldBe 4
      coinChange(Array(4, 5, 1), 8) shouldBe 2
    }
  }
}
