package hlib.liubchenko.topinterview150.trie

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_design_add_and_search_words_data_structure extends AnyWordSpec with Matchers {
  class WordDictionary() {

    def addWord(word: String): Unit = ???

    def search(word: String): Boolean = ???

  }

  "WordDictionary" should {
    "work as expected" in {
      pending

      val dictionary = new WordDictionary()
      dictionary.addWord("bad")
      dictionary.addWord("dad")
      dictionary.addWord("mad")

      dictionary.search("pad") shouldBe false
      dictionary.search("bad") shouldBe true
      dictionary.search(".ad") shouldBe true
      dictionary.search("b..") shouldBe true
    }
  }
}
