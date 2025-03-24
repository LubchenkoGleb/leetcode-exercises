package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _15_candy extends AnyWordSpec with Matchers {
  def candy(ratings: Array[Int]): Int = {
    val n = ratings.length
    if (n == 1) return 1
    val candies = Array.fill(n)(1)

    ratings.indices.sliding(2).foreach { case Seq(aI, bI) =>
      if (ratings(bI) > ratings(aI)) candies(bI) = candies(aI) + 1
    }
    ratings.indices.reverse.sliding(2).foreach { case Seq(aI, bI) =>
      if (ratings(aI) < ratings(bI)) candies(bI) = math.max(candies(bI), candies(aI) + 1)
    }

    candies.sum
  }

  "candy" should {
    "work as expected" in {
      candy(Array(1, 0, 2)) shouldBe 5 // 2, 1, 2
      candy(Array(1, 2, 2)) shouldBe 4 // 1, 2, 1
      candy(Array(1, 3, 2, 2, 1)) shouldBe 7 // 1, 2, 1, 2, 1
      candy(Array(1, 3, 4, 5, 2)) shouldBe 11 // 1, 2, 3, 4, 1
      candy(Array(1, 2, 87, 87, 87, 2, 1)) shouldBe 13 // 1, 2, 3, 1, 3, 2, 1
      candy(Array(1, 2, 3, 5, 4, 3, 2, 1, 4, 3, 2, 1)) shouldBe 31 // 1, 2, 3, 1, 3, 2, 1
    }
  }
}
