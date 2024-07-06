package hlib.liubchenko.topinterview150.one_dimensional_dp

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec


class _1_climbing_stairs extends AnyWordSpec with Matchers {
  def climbStairs(n: Int): Int = {
    // all 1
    // one 2 and rest 1 = n - 1
    // two 2 nad res 1 =

    // n = 4
    // 1 1 1 1
    // 2 1 1
    // 1 2 1
    // 1 1 2
    // 2 2
    // = 5 combs
    //
    // n = 5
    // 1 1 1 1 1
    // 2 1 1 1
    // 1 2 1 1
    // 1 1 2 1
    // 1 1 1 2
    // 2 2 1
    // 2 1 2
    // 1 2 2
    // = 8 combs

    // n = 6
    // 1 1 1 1 1 1
    // 2 1 1 1 1
    // 1 2 1 1 1
    // 1 1 2 1 1
    // 1 1 1 2 1
    // 1 1 1 1 2
    // 2 2 1 1
    // 2 1 2 1
    // 2 1 1 2
    // 1 2 1 2
    // 1 1 2 2
    // 1 2 2 1
    // 2 2 2
    // = 13 combs

//    val minLen = n / 2 + n % 2
//    val all2 = if (n % 2 == 0) 1 else 0
//    1 + all2 + (1 until minLen).map(n - _).sum
    import scala.annotation.tailrec

    @tailrec
    def loop(n1: Int, n2: Int, i: Int): Int = if (i == n) n1 + n2 else loop(n2, n1 + n2, i + 1)

    loop(0, 1, 1)
  }

  "climbStairs" should {
    "work as expected" in {
      climbStairs(4) shouldBe 5
      climbStairs(5) shouldBe 8
      climbStairs(6) shouldBe 13
    }
  }
}
