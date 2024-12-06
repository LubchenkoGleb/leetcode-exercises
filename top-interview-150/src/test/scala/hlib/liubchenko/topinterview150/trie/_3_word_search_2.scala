package hlib.liubchenko.topinterview150.trie

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_word_search_2 extends AnyWordSpec with Matchers {
  def findWords(board: Array[Array[Char]], words: Array[String]): List[String] = ???

  "findWords" should {
    "work as expected #1" in {
      pending

      findWords(
        board = Array(
          Array('o', 'a', 'a', 'n'),
          Array('e', 't', 'a', 'e'),
          Array('i', 'h', 'k', 'r'),
          Array('i', 'f', 'l', 'v')
        ),
        words = Array("oath", "pea", "eat", "rain")
      ) shouldBe List("eat", "oath")
    }

    "work as expected #2" in {
      pending

      findWords(
        board = Array(
          Array('a', 'b'),
          Array('c', 'd')
        ),
        words = Array("abcb")
      ) shouldBe List.empty
    }
  }
}
