package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _15_candy extends AnyWordSpec with Matchers {
  def candy(ratings: Array[Int]): Int = {
    val candies = Array.fill(ratings.length)(1)

    ratings.zipWithIndex.sliding(3).foreach { case Array((aV, aI), (bV, bI), (cV, cI)) =>
      if (aV > bV) candies(aI) += 1
      if (aV < bV) candies(bI) = candies(aI) + 1
      if (bV > cV) candies(bI) = candies(cI) + 1
      if (cV > bV) candies(cI) = candies(bI) + 1
      println(candies.mkString(", "))
    }

    println("before: " + candies.mkString(", "))

    candies.zipWithIndex.sliding(3).foreach { case Array((aV, aI), (bV, bI), (cV, cI)) =>
      if (aV < bV && bV > cV) candies(bI) = math.max(aV, cV) + 1
    }

    println("after: " + candies.mkString(", "))

    candies.sum
  }

  "candy" should {
    "work as expected" in {
//      candy(Array(1, 0, 2)) shouldBe 5 // 2, 1, 2
//      candy(Array(1, 2, 2)) shouldBe 4 // 1, 2, 1
//      candy(Array(1, 3, 2, 2, 1)) shouldBe 7 // 1, 2, 1, 2, 1
      candy(Array(1, 3, 4, 5, 2)) shouldBe 11 // 1, 2, 3, 4, 1
//      candy(Array(1, 2, 87, 87, 87, 2, 1)) shouldBe 13 // 1, 2, 3, 1, 3, 2, 1
    }
  }
}
