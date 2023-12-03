package hlib.liubchenko.leetcode75.binary_search

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_successful_pairs_of_spells_and_potions extends AnyWordSpec with Matchers {
  import scala.annotation.tailrec

  // My, Runtime 50, Memory 50
  def successfulPairs_2(spells: Array[Int], potions: Array[Int], success: Long): Array[Int] = {
    potions.sortInPlace

    @tailrec
    def findPotionPos(strength: Long, res: Int, lowI: Int, highI: Int): Int = {
      println(s"findPotionPos: strength: $strength, res: $res, lowI: $lowI, highI: $highI")
      if (highI <= lowI) if (potions(lowI) >= strength) lowI else res
      else {
        val mid = lowI + (highI - lowI) / 2
        println(s"mid: $mid")
        if (potions(mid) < strength) findPotionPos(strength, res, mid + 1, highI)
        else if (potions(mid) > strength) findPotionPos(strength, mid, lowI, mid - 1)
        else (lowI to mid).find(potions(_) == strength).getOrElse(mid)
      }
    }

    spells.map(spellStrength => {
      val minPotionStrength = success / spellStrength + (if (success % spellStrength == 0) 0 else 1)
      val pos = findPotionPos(minPotionStrength, potions.length, 0, potions.length - 1)
      println(s"pos: $pos")
      potions.length - pos
    })
  }

  // My, Runtime 50, Memory 50
  def successfulPairs(spells: Array[Int], potions: Array[Int], success: Long): Array[Int] = {
    potions.sortInPlace

    @tailrec
    def findPotionPos(strength: Long, left: Int, right: Int): Int =
      if (right < left) left
      else {
        val mid = left + (right - left) / 2
        println(s"findPotionPos: strength: $strength, left: $left, right: $right, mid: $mid")
        if (potions(mid) >= strength) findPotionPos(strength, left, mid - 1)
        else findPotionPos(strength, mid + 1, right)
      }

    spells.map { spellStrength =>
      val minPotionStrength = success / spellStrength + (if (success % spellStrength == 0) 0 else 1)
      val pos = findPotionPos(minPotionStrength, 0, potions.length - 1)
      println(s"pos: $pos")
      potions.length - pos
    }
  }

  "successfulPairs" should {
    "work as expected" in {
      successfulPairs(Array(5, 1, 3), Array(1, 2, 3, 4, 5), 7) shouldBe Array(4, 0, 3)
      println("")
      successfulPairs(Array(3, 1, 2), Array(8, 5, 8), 16) shouldBe Array(2, 0, 2)
      println("")
      successfulPairs(Array(9, 39), Array(35, 40, 22, 37, 29, 22), 320) shouldBe Array(2, 6)
    }
  }
}
