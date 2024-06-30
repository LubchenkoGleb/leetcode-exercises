package hlib.liubchenko.topinterview150.sliding_window

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_longest_substring_without_repeating_characters extends AnyWordSpec with Matchers {
  def lengthOfLongestSubstring(s: String): Int = {
    import scala.collection.mutable

    val window = mutable.HashMap.empty[Char, Int]
    s.zipWithIndex.foldLeft(0) { case (acc, (c, i)) =>
      window.put(c, i) match {
        case Some(cPos) => window.filterInPlace { case (_, wI) => wI > cPos }; acc
        case None       => math.max(acc, window.size)
      }
    }
  }

  "lengthOfLongestSubstring" should {
    "work as expected" in {
      lengthOfLongestSubstring("abcabcbb") shouldBe 3
    }
  }
}
