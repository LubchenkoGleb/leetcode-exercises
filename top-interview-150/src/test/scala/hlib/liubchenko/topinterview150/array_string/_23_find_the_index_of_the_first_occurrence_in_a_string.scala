package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _23_find_the_index_of_the_first_occurrence_in_a_string extends AnyWordSpec with Matchers {
  def strStr(haystack: String, needle: String): Int = {
    import scala.annotation.tailrec

    @tailrec
    def check(i: Int, j: Int): Int =
      if (j == needle.length) i - j
      else if (i == haystack.length) -1
      else if (haystack(i) == needle(j)) check(i + 1, j + 1)
      else check(i - j + 1, 0)

    check(0, 0)
  }

  "strStr" should {
    "work as expected" in {
//      strStr("sadbutsad", "sad") shouldBe 0
//      strStr("leetcode", "leeto") shouldBe -1
      strStr("mississippi", "issip") shouldBe 4
    }
  }
}
