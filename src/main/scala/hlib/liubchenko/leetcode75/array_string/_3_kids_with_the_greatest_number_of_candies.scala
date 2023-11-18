package hlib.liubchenko.leetcode75.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_kids_with_the_greatest_number_of_candies extends AnyWordSpec with Matchers {
  def kidsWithCandies(candies: Array[Int], extraCandies: Int): List[Boolean] = {
    val maxWithoutExtra = candies.max
    candies.map(_ + extraCandies >= maxWithoutExtra).toList
  }

  "kidsWithCandies" should {
    "work" in {
      kidsWithCandies(Array(2, 3, 5, 1, 3), 3) shouldBe List(true, true, true, false, true)
    }
  }
}
