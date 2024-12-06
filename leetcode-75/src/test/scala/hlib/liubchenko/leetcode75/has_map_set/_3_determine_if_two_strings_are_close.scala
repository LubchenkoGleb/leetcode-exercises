package hlib.liubchenko.leetcode75.has_map_set

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_determine_if_two_strings_are_close extends AnyWordSpec with Matchers {
  def countElements[A](iterable: Iterable[A]): Map[A, Int] = iterable.groupMapReduce(c => c)(_ => 1)(_ + _)

  // My, Runtime 66, Memory 33
  def closeStrings(word1: String, word2: String): Boolean =
    if (word1 == word2) true
    else if (word1.length != word2.length) false
    else {
      val w1Letters = countElements(word1)
      val w2Letters = countElements(word2)
      w1Letters.keys == w2Letters.keys && countElements(w1Letters.values) == countElements(w2Letters.values)
    }

  "closeStrings" should {
    "work as expected" in {
      closeStrings("abc", "bca") shouldBe true
      closeStrings("a", "aa") shouldBe false
      closeStrings("cabbba", "abbccc") shouldBe true
      closeStrings("cabbba", "aabbss") shouldBe false
    }
  }
}
