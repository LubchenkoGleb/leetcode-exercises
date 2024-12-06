package hlib.liubchenko.leetcode75.sliding_window

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_maximum_number_of_vowels_in_a_substring_of_given_length extends AnyWordSpec with Matchers {
  val vowels = Set('a', 'e', 'i', 'o', 'u')

  // Mine, Runtime 40, Memory 73
  def maxVowels(s: String, k: Int): Int = {
    var max = (0 until k).map(s).count(vowels.contains)
    var count = max
    for (i <- k until s.length) {
      if (vowels.contains(s(i))) count += 1
      if (vowels.contains(s(i - k))) count -= 1
      if (count > max) max = count
    }
    max
  }

  "maxVowels" should {
    "work as expected" in {
      maxVowels("abciiidef", 3) shouldBe 3
      maxVowels("aeiou", 2) shouldBe 2
      maxVowels("leetcode", 3) shouldBe 2
    }
  }
}
