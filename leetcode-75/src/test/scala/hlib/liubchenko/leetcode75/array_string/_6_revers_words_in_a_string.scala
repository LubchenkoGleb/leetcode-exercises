package hlib.liubchenko.leetcode75.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _6_revers_words_in_a_string extends AnyWordSpec with Matchers {
  def reverseWords(s: String): String =
    s.split("\\s+").filterNot(_.isEmpty).reverse.mkString(" ")

  "reverseWords" should {
    "work as expected" in {
      reverseWords("the sky is blue") shouldBe "blue is sky the"
      reverseWords("  hello world  ") shouldBe "world hello"
      reverseWords("a good   example") shouldBe "example good a"
    }
  }
}
