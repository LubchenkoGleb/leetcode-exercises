package hlib.liubchenko.topinterview150.sliding_window

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _4_minimum_window_substring extends AnyWordSpec with Matchers {
  def minWindow(s: String, t: String): String = ???

  "minWindow" should {
    "work as expected" in {
      minWindow("ADOBECODEBANC", "ABC") shouldBe "BANC"
      minWindow("a", "a") shouldBe "a"
      minWindow("a", "aa") shouldBe ""
    }
  }
}
