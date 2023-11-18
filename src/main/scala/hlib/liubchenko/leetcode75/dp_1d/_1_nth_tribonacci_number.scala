package hlib.liubchenko.leetcode75.dp_1d

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_nth_tribonacci_number extends AnyWordSpec with Matchers {
  // Mine, Runtime 75, Memory 66
  def tribonacci(n: Int): Int = {
    import scala.annotation.tailrec

    @tailrec
    def loop(count: Int, n1: Int, n2: Int, n3: Int): Int =
      if (count == n) n3
      else loop(count + 1, n2, n3, n1 + n2 + n3)

    if (n == 0) 0
    else if (n == 1) 1
    else loop(2, 0, 1, 1)
  }

  "tribonacci" should {
    "work as expected" in {
      tribonacci(0) shouldBe 4
      tribonacci(4) shouldBe 4
      tribonacci(25) shouldBe 1389537
    }
  }
}
