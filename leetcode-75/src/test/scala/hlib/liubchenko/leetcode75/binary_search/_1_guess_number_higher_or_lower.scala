package hlib.liubchenko.leetcode75.binary_search

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_guess_number_higher_or_lower extends AnyWordSpec with Matchers {
  import scala.annotation.tailrec

  sealed trait Solution {
    def pick: Int

    def guess(num: Int): Int =
      if (num > pick) -1
      else if (num < pick) 1
      else 0

    def guessNumber(n: Int): Int
  }

  class Solution1(override val pick: Int) extends Solution {
    def guessNumber(n: Int): Int = {
      @tailrec
      def loop(low: Int, high: Int): Int = {
        println(s"low: $low, high: $high")
        if (low == high) low
        else {
          val mid = ((low.toLong + high) / 2).toInt
          val res = guess(mid)
          if (res == 0) mid
          else if (res == -1) loop(low, mid - 1)
          else loop(mid + 1, high)
        }
      }

      loop(1, n)
    }
  }

  // Mine, Runtime 100, Memory 42
  class Solution2(override val pick: Int) extends Solution {
    def guessNumber(n: Int): Int = {
      @tailrec
      def loop(low: Int, high: Int): Int = if (low == high) low
      else {
        val mid = low + (high - low) / 2
        guess(mid) match {
          case 0 => mid
          case 1 => loop(mid + 1, high)
          case _ => loop(low, mid - 1)
        }
      }

      loop(1, n)
    }
  }

  "Solution" should {
    "work as expected" in {
      List[Int => Solution](new Solution1(_), new Solution2(_)).map { createSolution =>
        createSolution(1).guessNumber(1) shouldBe 1
        createSolution(2).guessNumber(2) shouldBe 2
        createSolution(6).guessNumber(10) shouldBe 6
        createSolution(50).guessNumber(1000) shouldBe 50
        createSolution(1).guessNumber(3) shouldBe 1
        createSolution(4).guessNumber(30) shouldBe 4
        createSolution(1_702_766_719).guessNumber(2_126_753_390) shouldBe 1_702_766_719
      }
    }
  }
}
