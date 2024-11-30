package hlib.liubchenko.topinterview150.sliding_window

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_substring_with_concatenation_of_all_words extends AnyWordSpec with Matchers {
  def findSubstring(s: String, words: Array[String]): List[Int] = ???

  "findSubstring" should {
    "work as expected" in {
      findSubstring("barfoothefoobarman", Array("foo", "bar")) shouldBe Array(0, 9)
      findSubstring("wordgoodgoodgoodbestword", Array("word", "good", "best", "word")) shouldBe List.empty
      findSubstring("barfoofoobarthefoobarman", Array("bar", "foo", "the")) shouldBe List(6, 9, 12)
    }
  }
}
