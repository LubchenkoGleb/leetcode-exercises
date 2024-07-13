package hlib.liubchenko.topinterview150.math

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_factorial_trailing_zeroes extends AnyWordSpec with Matchers {
  def trailingZeroes_2(n: Int): Int = {
    import scala.annotation.tailrec

    @tailrec
    def lastNonZeroDigitAndTrailingZeros(n: Int, zeroAcc: Int): (Int, Int) =
      if (n == 0) (0, zeroAcc)
      else if (n % 10 == 0) lastNonZeroDigitAndTrailingZeros(n / 10, zeroAcc + 1)
      else (n % 10, zeroAcc)

    (1 to n)
      .foldLeft((1, 0)) { case ((prevNonZeroDigit, zerAcc), value) =>
        val (lastDigit, endZeros) = lastNonZeroDigitAndTrailingZeros(value, 0)
        val (newLastNonZeroDigit, additionalZeror) = lastNonZeroDigitAndTrailingZeros(prevNonZeroDigit * lastDigit, 0)
        (newLastNonZeroDigit, additionalZeror + zerAcc + endZeros)
      }
      ._2
  }

  def trailingZeroes(n: Int): Int = {
    def flor5(n: Int): Int =
      if (n < 5) 0 else n / 5 + flor5(n / 5)

    flor5(n)
  }

  "trailingZeroes" should {
    "work as expected" in {
      trailingZeroes(5) shouldBe 1
      trailingZeroes(6) shouldBe 1
      trailingZeroes(7) shouldBe 1
      trailingZeroes(8) shouldBe 1
      trailingZeroes(9) shouldBe 1
      trailingZeroes(10) shouldBe 2
      trailingZeroes(20) shouldBe 4
      trailingZeroes(100) shouldBe 24
    }
  }
}
