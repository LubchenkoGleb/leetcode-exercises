package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _20_longest_common_prefix extends AnyWordSpec with Matchers {
  def longestCommonPrefix(strs: Array[String]): String = ???

  "longestCommonPrefix" should {
    "work as expected" in {
      longestCommonPrefix(Array("flower", "flow", "flight")) shouldBe "fl"
      longestCommonPrefix(Array("dog", "racecar", "car")) shouldBe ""
    }
  }
}
