package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _20_longest_common_prefix extends AnyWordSpec with Matchers {
  def longestCommonPrefix(strs: Array[String]): String = {
    def intersect(a: String, b: String) = {
      var commonLen = 0
      while (commonLen < a.length && commonLen < b.length && a(commonLen) == b(commonLen)) commonLen += 1
      a.take(commonLen)
    }

    strs.foldLeft(strs.head)(intersect)
  }

  "longestCommonPrefix" should {
    "work as expected" in {
      longestCommonPrefix(Array("flower", "flow", "flight")) shouldBe "fl"
      longestCommonPrefix(Array("dog", "racecar", "car")) shouldBe ""
    }
  }
}
