package hlib.liubchenko.topinterview150.math

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _4_sqrt_x extends AnyWordSpec with Matchers {
  def mySqrt(x: Int): Int = {
    val xd = x.toDouble;
    val error = 0.00001
    var s = xd

    while (s - (xd / s) > error)
      s = (s + xd / s) / 2

    s.toInt
  }

  "mySqrt" should {
    "work as expected" in {
      mySqrt(8) shouldBe 2
      mySqrt(2147395599) shouldBe 46339
    }
  }
}
