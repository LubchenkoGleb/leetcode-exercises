package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _23_find_the_index_of_the_first_occurrence_in_a_string extends AnyWordSpec with Matchers {
  def strStr(haystack: String, needle: String): Int = ???

  "strStr" should {
    "work as expected" in {
      strStr("sadbutsad", "sad") shouldBe 0
      strStr("leetcode", "leeto") shouldBe -1
    }
  }
}
