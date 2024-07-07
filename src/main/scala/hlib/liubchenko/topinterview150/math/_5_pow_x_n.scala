package hlib.liubchenko.topinterview150.math

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec


class _5_pow_x_n extends AnyWordSpec with Matchers {
  def myPow(x: Double, n: Int): Double = {
    import scala.annotation.tailrec

    @tailrec
    def loop(x: Double, n: Int, acc: Double): Double =
      if (n == 0) acc
      else if (n % 2 == 0) loop(x * x, n / 2, acc)
      else if (n > 0) loop(x * x, n / 2, acc * x)
      else loop(x * x, n / 2, acc / x)

    loop(x, n, 1d)
  }

  def myPow_memLimit(x: Double, n: Int): Double = {
    if (n >= 0) (1 to n).foldLeft(1d) { case (acc, _) => acc * x }
    else (n to -1).foldLeft(1d) { case (acc, _) => acc / x }
  }

  "myPow" should {
    "work as expected" in {
      myPow(3, 4) shouldBe 81
      myPow(2, -2) shouldBe 0.25
    }
  }
}
