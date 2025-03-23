package hlib.liubchenko.topinterview150.multi_dimensional_dp

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _4_longest_palindromic_substring extends AnyWordSpec with Matchers {
  import scala.annotation.tailrec

  def longestPalindrome(s: String): String = {
    @tailrec
    def palindromeLen(from: Int, to: Int): (Int, Int) =
      if (from < 0 || to >= s.length || s(from) != s(to)) (from + 1, to - 1)
      else palindromeLen(from - 1, to + 1)

    var (maxFrom, maxTo) = (0, 0)
    s.indices.foreach { i =>
      val (oddFrom, oddTo) = palindromeLen(i, i)
      val (evenFom, evenTo) = palindromeLen(i, i + 1)

      if (oddTo - oddFrom > maxTo - maxFrom) { maxFrom = oddFrom; maxTo = oddTo }
      if (evenTo - evenFom > maxTo - maxFrom) { maxFrom = evenFom; maxTo = evenTo }
    }

    s.substring(maxFrom, maxTo + 1)
  }

  def longestPalindrome_brute_force(s: String): String = {
    @tailrec
    def isPalindrome(from: Int, to: Int): Boolean = from >= to || (s(from) == s(to) && isPalindrome(from + 1, to - 1))

    var (from, to) = (0, 0)
    for { i <- s.indices; j <- 0 to i; if isPalindrome(j, i) && i - j > to - from } { from = j; to = i }
    s.substring(from, to + 1)
  }

  "longestPalindrome" should {
    "work as expected" in {
      longestPalindrome("abcba") shouldBe "abcba"
      longestPalindrome("babad") shouldBe "bab"
      longestPalindrome("cbbd") shouldBe "bb"
    }
  }
}
