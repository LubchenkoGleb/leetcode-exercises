package hlib.liubchenko.topinterview150.math

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_palindrome_number extends AnyWordSpec with Matchers {
  def isPalindrome(x: Int): Boolean = {
    import scala.annotation.tailrec

    @tailrec
    def reverseNumber(rem: Int, res: Int): Int =
      if (rem == 0) res
      else reverseNumber(rem / 10, res * 10 + rem % 10)

    if (x < 0) false
    else x == reverseNumber(x, 0)
  }

  def isPalindrome_as_str(x: Int): Boolean = {
    val xStr = x.toString
    var (start, end, res) = (0, xStr.length - 1, true)
    while (start < end && res) {
      res = xStr(start) == xStr(end)
      start += 1; end -= 1
    }
    res
  }

  "isPalindrome" should {
    "work as expected" in {
      isPalindrome(121) shouldBe true
      isPalindrome(-121) shouldBe false
    }
  }
}
