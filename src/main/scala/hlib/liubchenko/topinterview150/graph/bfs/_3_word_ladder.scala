package hlib.liubchenko.topinterview150.graph.bfs

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.annotation.tailrec

class _3_word_ladder extends AnyWordSpec with Matchers {
  def ladderLength(beginWord: String, endWord: String, wordList: List[String]): Int = {
    def isSimilar(usedWords: Set[String])(word: String): Boolean =
      usedWords.exists(_.zip(word).count { case (a, b) => a != b } == 1)

    @tailrec
    def loop(used: Set[String], rem: Set[String], count: Int): Int =
      if (used.contains(endWord)) count + 1
      else if (rem.isEmpty || used.isEmpty) 0
      else {
        val next = rem.filter(isSimilar(used))
        loop(next, rem -- next, count + 1)
      }

    loop(Set(beginWord), wordList.filter(_.length == beginWord.length).toSet, 0)
  }

  // Brute Force, Memory Limit
  def ladderLength_2(beginWord: String, endWord: String, wordList: List[String]): Int = {
    def availableTransformation(word: String, wordList: Set[String]): Set[String] =
      wordList.filter {
        _.zip(word).count { case (a, b) => a != b } == 1
      }

    def loop(word: String, wordList: Set[String]): Int =
      if (word == endWord) 1
      else if (wordList.isEmpty) Int.MaxValue
      else
        availableTransformation(word, wordList)
          .map(nextWord => loop(nextWord, wordList - nextWord))
          .collect { case res if res != Int.MaxValue => res + 1 }
          .minOption
          .getOrElse(Int.MaxValue)

    val res = loop(beginWord, wordList.filter(_.length == beginWord.length).toSet)
    if (res == Int.MaxValue) 0 else res
  }

  "ladderLength" should {
    "work as expected" in {
      ladderLength("hit", "cog", List("hot", "dot", "dog", "lot", "log", "cog")) shouldBe 5

      //@formatter:off
      val input = List(
        "si", "go", "se", "cm", "so", "ph", "mt", "db", "mb", "sb", "kr", "ln", "tm", "le", "av", "sm", "ar", "ci",
        "ca", "br", "ti", "ba", "to", "ra", "fa", "yo", "ow", "sn", "ya", "cr", "po", "fe", "ho", "ma", "re", "or",
        "rn", "au", "ur", "rh", "sr", "tc", "lt", "lo", "as", "fr", "nb", "yb", "if", "pb", "ge", "th", "pm", "rb",
        "sh", "co", "ga", "li", "ha", "hz", "no", "bi", "di", "hi", "qa", "pi", "os", "uh", "wm", "an", "me", "mo",
        "na", "la", "st", "er", "sc", "ne", "mn", "mi", "am", "ex", "pt", "io", "be", "fm", "ta", "tb", "ni", "mr",
        "pa", "he", "lr", "sq", "ye"
      )
      //@formatter:on
      ladderLength("qa", "sq", input) shouldBe 5

      ladderLength("hit", "cog", List("hot", "dot", "dog", "lot", "log")) shouldBe 0

      ladderLength("hot", "dog", List("hot", "dog")) shouldBe 0
    }
  }
}
