package hlib.liubchenko.topinterview150.trie

import hlib.liubchenko.topinterview150.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_design_add_and_search_words_data_structure extends AnyWordSpec with Matchers {
  class WordDictionary() {
    import scala.annotation.tailrec
    import scala.collection.mutable

    class Trie(val values: mutable.Map[Char, Trie] = mutable.Map.empty, var isWord: Boolean = false)
    val trie = new Trie()

    def addWord(word: String): Unit = {
      @tailrec
      def loop(trie: Trie, word: String): Unit = if (word.nonEmpty) {
        val toUpdate = trie.values.getOrElseUpdate(word.head, new Trie())
        toUpdate.isWord = toUpdate.isWord || word.length == 1
        loop(toUpdate, word.tail)
      }
      loop(trie, word)
    }

    def search(word: String): Boolean = {
      def loop(trie: Trie, word: String): Boolean =
        if (word.isEmpty && trie.isWord) true
        else if (word.nonEmpty && word.head != '.' && trie.values.contains(word.head))
          loop(trie.values(word.head), word.tail)
        else if (word.nonEmpty && word.head == '.') trie.values.exists { case (_, t) => loop(t, word.tail) }
        else false

      loop(trie, word)
    }
  }

  "WordDictionary" should {
    "work as expected" in {
      val dictionary = new WordDictionary()
      dictionary.addWord("bad")
      dictionary.addWord("dad")
      dictionary.addWord("mad")

      dictionary.search("pad") shouldBe false
      dictionary.search("bad") shouldBe true
      dictionary.search(".ad") shouldBe true
      dictionary.search("b..") shouldBe true

      dictionary.addWord("b")
      dictionary.search("b") shouldBe true
    }

    "work as expected #2" in {
      val dictionary = new WordDictionary()

      val cmd :: value :: res :: Nil = Utils.readInputFile("trie/_2_design_add_and_search_words_data_structure.txt")
      val input = cmd.split(",").zip(value.split(",")).zip(res.split(","))
      input.foreach {
        case (("addWord", v), _) =>
          println(s"addWord($v)")
          dictionary.addWord(v)
        case (("search", v), r) =>
          println(s"search($v) shouldBe $r")
          dictionary.search(v) shouldBe r.toBoolean
      }
    }

    "work as expected #3" in {
      val dictionary = new WordDictionary()
      dictionary.addWord("a")
      dictionary.addWord("ab")

      dictionary.search(".") shouldBe true
      dictionary.search("..") shouldBe true
      dictionary.search(".b") shouldBe true
      dictionary.search("a.") shouldBe true
      dictionary.search("...") shouldBe false
    }

    "work as expected #4" in {
      val dictionary = new WordDictionary()

      dictionary.addWord("a")
      dictionary.addWord("ab")
//      dictionary.search("a") shouldBe false
//      dictionary.search("a.") shouldBe true
//      dictionary.search("ab") shouldBe true
//      dictionary.search(".a") shouldBe false
//      dictionary.search(".b") shouldBe true
//      dictionary.search("ab.") shouldBe false
      dictionary.search(".") shouldBe true
//      dictionary.search("..") shouldBe true
    }
  }
}
