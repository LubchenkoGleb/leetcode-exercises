package hlib.liubchenko.leetcode75.prefix_sum

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_find_the_highest_altitude extends AnyWordSpec with Matchers {
  // Mine, Runtime 47, Memory 47
  def largestAltitude(gain: Array[Int]): Int = {
    var maxAlt, curAlt = 0
    gain.foreach { i =>
      curAlt += i
      maxAlt = math.max(maxAlt, curAlt)
    }
    maxAlt
  }

  // Mine, Runtime 23, Memory 61
  def largestAltitude_2(gain: Array[Int]): Int =
    gain
      .foldLeft(0 -> 0) { case ((maxAlt, currAlt), altitude) =>
        val newCurGain = currAlt + altitude
        math.max(maxAlt, newCurGain) -> newCurGain
      }
      ._1

  // Mine, Runtime 28, Memory 14
  def largestAltitude_1(gain: Array[Int]): Int =
    gain.scan(0)(_ + _).max

  "largestAltitude" should {
    "work" in {
      largestAltitude(Array(-5, 1, 5, 0, -7)) shouldBe 1
      largestAltitude(Array(-4, -3, -2, -1, 4, 3, 2)) shouldBe 0
    }
  }
}
