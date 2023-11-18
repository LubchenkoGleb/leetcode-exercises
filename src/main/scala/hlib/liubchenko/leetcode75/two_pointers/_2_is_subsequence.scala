package hlib.liubchenko.leetcode75.two_pointers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_is_subsequence extends AnyWordSpec with Matchers {
  sealed trait Solution {
    def isSubsequence(s: String, t: String): Boolean
  }
  // Mine, Runtime 62, Memory 67
  object Solution1 extends Solution {
    def isSubsequence(s: String, t: String): Boolean = {
      var sI = 0
      for (c <- t if sI < s.length && c == s(sI)) sI += 1
      sI == s.length
    }
  }

  // Mine, Runtime 12, Memory 55
  object Solution2 extends Solution {
    import scala.annotation.tailrec

    def isSubsequence(s: String, t: String): Boolean = {
      // Mine, Runtime 57, Memory 25
      val sL = s.length
      val tL = t.length
      @tailrec
      def loop(sI: Int, tI: Int): Boolean =
        if (sI >= sL) true
        else if (tI >= tL) false
        else if (s(sI) == t(tI)) loop(sI + 1, tI + 1)
        else loop(sI, tI + 1)

      loop(0, 0)
    }
  }

  "All solutions" should {
    "work as expected" in {
      List(Solution1, Solution2).foreach { solution =>
        solution.isSubsequence("abc", "ahbgdc") shouldBe true
        solution.isSubsequence("axc", "ahbgdc") shouldBe false
      }
    }
  }
}
