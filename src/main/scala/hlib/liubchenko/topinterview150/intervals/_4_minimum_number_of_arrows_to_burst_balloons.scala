package hlib.liubchenko.topinterview150.intervals

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _4_minimum_number_of_arrows_to_burst_balloons extends AnyWordSpec with Matchers {

  def findMinArrowShots(points: Array[Array[Int]]): Int =
    points
      .sortInPlaceBy(_(0))
      .foldLeft(1, Array(Int.MinValue, Int.MaxValue)) { case ((arrowsAcc, shotRangeAcc), balloon) =>
        intersect(shotRangeAcc, balloon) match {
          case Some(value) => (arrowsAcc, value)
          case None        => (arrowsAcc + 1, balloon)
        }
      }
      ._1

  def intersect(a: Array[Int], b: Array[Int]): Option[Array[Int]] =
    if (a(1) >= b(0)) Some(Array(math.max(a(0), b(0)), math.min(a(1), b(1))))
    else None

  "findMinArrowShots" should {
    "work as expected" in {
      findMinArrowShots(Array(Array(10, 16), Array(2, 8), Array(1, 6), Array(7, 12))) shouldBe 2
      findMinArrowShots(Array(Array(1, 2), Array(3, 4), Array(5, 6), Array(7, 8))) shouldBe 4
      findMinArrowShots(Array(Array(1, 2), Array(2, 3), Array(3, 4), Array(4, 5))) shouldBe 2
    }
  }
}
