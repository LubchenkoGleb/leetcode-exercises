package hlib.liubchenko.leetcode75.queue

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_number_of_recent_calls extends AnyWordSpec with Matchers {
  class RecentCounter() {
    import scala.collection.mutable

    // Mine, Runtime 52 Memory 30
    // private val q = mutable.Queue.empty[Int]
    // Not Mine, Runtime 52 Memory 86
    private val q = mutable.ArrayDeque.empty[Int]

    def ping(t: Int): Int = {
      val oldestAllowed = t - 3000
      q.addOne(t).dropWhileInPlace { _ < oldestAllowed }.length
    }
  }

  "RecentCounter" should {
    "work as expected" in {
      val counter = new RecentCounter()
      List(1, 100, 3001, 3002).map(counter.ping) shouldBe List(1, 2, 3, 3)
    }
  }
}
