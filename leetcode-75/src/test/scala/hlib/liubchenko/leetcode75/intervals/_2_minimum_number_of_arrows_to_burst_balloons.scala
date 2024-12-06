package hlib.liubchenko.leetcode75.intervals

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class _2_minimum_number_of_arrows_to_burst_balloons extends AnyWordSpec with Matchers {
  def findMinArrowShots(points: Array[Array[Int]]): Int = {
    val sortedPoints = mutable.Queue.from(points.sortInPlaceBy(_(0)))

    def loop(interval: Array[Int]): Unit = {
      val (from, to) = (interval(0), interval(1))
//      val bestPoint = (from to to).map { i => sortedPoints.m}
    }

    ???
  }

  "findMinArrowShots" should {
    "work as an expected" in {

    }
  }
}
