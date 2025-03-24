package hlib.liubchenko.topinterview150.heap

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_ipo extends AnyWordSpec with Matchers {
  def findMaximizedCapital(k: Int, w: Int, profits: Array[Int], capital: Array[Int]): Int = {
    import scala.annotation.tailrec
    import scala.collection.mutable

    val maxAvailableProfits = mutable.PriorityQueue.empty[Int]
    val indicesOrderedByCapital = capital.indices.sortBy(capital)

    @tailrec
    def loop(k: Int, w: Int, i: Int): Int =
      if (k == 0) w
      else {
        var nI = i
        while (nI < profits.length && capital(indicesOrderedByCapital(nI)) <= w) {
          maxAvailableProfits.enqueue(profits(indicesOrderedByCapital(nI)))
          nI += 1
        }
        if (maxAvailableProfits.isEmpty) w
        else loop(k - 1, w + maxAvailableProfits.dequeue(), nI)
      }

    loop(k, w, 0)
  }

  def findMaximizedCapital_v1(k: Int, w: Int, profits: Array[Int], capital: Array[Int]): Int = {
    import scala.annotation.tailrec
    import scala.collection.mutable.ArrayBuffer

    val dealIdsOrderedByProfit: ArrayBuffer[Int] =
      ArrayBuffer.from(profits.zipWithIndex.sortBy(_._1)(Ordering[Int].reverse).map(_._2))

    @tailrec
    def loop(k: Int, w: Int): Int =
      if (k == 0) w
      else
        dealIdsOrderedByProfit.find(i => capital(i) <= w) match {
          case Some(bestAvailableDealId) =>
            dealIdsOrderedByProfit -= bestAvailableDealId
            loop(k - 1, w + profits(bestAvailableDealId))
          case None => w
        }

    loop(k, w)
  }

  "findMaximizedCapital" should {
    "work as expected" in {
      findMaximizedCapital(k = 2, w = 0, profits = Array(1, 2, 3), capital = Array(0, 1, 1)) shouldBe 4
      findMaximizedCapital(k = 3, w = 0, profits = Array(1, 2, 3), capital = Array(0, 1, 2)) shouldBe 6
    }
  }
}
