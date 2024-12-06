package hlib.liubchenko.topinterview150.one_dimensional_dp

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_word_break extends AnyWordSpec with Matchers {
  def wordBreak(s: String, wordDict: List[String]): Boolean = {
    import scala.collection.mutable

    val cache = mutable.Map.empty[Int, Boolean]

    def loop(s: String): Boolean = {
      def updateCache = wordDict.exists(w => s.startsWith(w) && loop(s.substring(w.length)))
      s.isEmpty || cache.getOrElseUpdate(s.length, updateCache)
    }

    loop(s)
  }

  def wordBreak_2(s: String, wordDict: List[String]): Boolean = {
    var (start, wordSet) = (0, wordDict.toSet)
    s.indices.foreach { i =>
      val word = s.substring(start, i + 1)
      if (wordSet.contains(word)) start = i + 1
    }
    start == s.length
  }

  "wordBreak" should {
    "work as expected" in {
      wordBreak("leetcode", List("leet", "code")) shouldBe true
      wordBreak("applepenapple", List("apple", "pen")) shouldBe true
      wordBreak("catsandog", List("cats", "dog", "sand", "and", "cat")) shouldBe false
      wordBreak("aaaaaaa", List("aaaa", "aaa")) shouldBe true
    }
  }
}
