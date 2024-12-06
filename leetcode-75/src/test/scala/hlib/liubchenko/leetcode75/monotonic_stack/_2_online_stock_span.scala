package hlib.liubchenko.leetcode75.monotonic_stack

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class StockSpanner() {
  private val stack = new mutable.Stack[Int]()

  def next(price: Int): Int = {
    stack.push(price)

    var res = 0
    stack.find { i =>
      if (i > price) true else { res += 1; false }
    }
    res
  }

}

class _2_online_stock_span extends AnyWordSpec with Matchers {
  "work as expected" should {
    "work as expected" in {
      val spanner = new StockSpanner()
      spanner.next(100) shouldBe 1
      spanner.next(80) shouldBe 1
      spanner.next(60) shouldBe 1
      spanner.next(70) shouldBe 2
      spanner.next(60) shouldBe 1
      spanner.next(75) shouldBe 4
      spanner.next(85) shouldBe 6
    }
  }
}
