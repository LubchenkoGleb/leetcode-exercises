package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _19_length_of_last_word extends AnyWordSpec with Matchers {
  def lengthOfLastWord(s: String): Int = {
    var lastWordStart = -1
    var lastWordEnd = -1
    var i = s.length - 1

    while (lastWordStart == -1) {
      val c = s(i)

      if (lastWordEnd == -1 && c != ' ') { lastWordEnd = i }
      if (lastWordEnd != -1 && c == ' ') lastWordStart = i + 1
      else if (i == 0) lastWordStart = 0

      i -= 1
    }

    lastWordEnd - lastWordStart + 1
  }

  "lengthOfLastWord" should {
    "work as expected" in {
      lengthOfLastWord(" moon  ") shouldBe 4
      lengthOfLastWord("   fly me   to   the moon  ") shouldBe 4
      lengthOfLastWord("a") shouldBe 1
      lengthOfLastWord("day") shouldBe 3
    }
  }
}
