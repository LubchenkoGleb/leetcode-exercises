package hlib.liubchenko.leetcode75.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _4_can_place_flowers extends AnyWordSpec with Matchers {
  def canPlaceFlowers(flowerbed: Array[Int], n: Int): Boolean = {
    var count = 0
    flowerbed.indices.foreach { i =>
      val prev = if (i == 0) flowerbed.head else flowerbed(i - 1)
      val next = if (i == flowerbed.length - 1) flowerbed.last else flowerbed(i + 1)
      if (prev == 0 && flowerbed(i) == 0 && next == 0) {
        count += 1
        flowerbed(i) = 1
      }
    }
    count >= n
  }

  "canPlaceFlowers" should {
    "work as expected" in {
      canPlaceFlowers(Array(1, 0, 0, 0, 1), n = 1) shouldBe true
      canPlaceFlowers(Array(1, 0, 0, 0, 1), n = 2) shouldBe false
      canPlaceFlowers(Array(1, 0, 1, 0, 1, 0, 1), n = 0) shouldBe true
      canPlaceFlowers(Array(0, 0, 1, 0, 0), n = 1) shouldBe true
    }
  }
}
