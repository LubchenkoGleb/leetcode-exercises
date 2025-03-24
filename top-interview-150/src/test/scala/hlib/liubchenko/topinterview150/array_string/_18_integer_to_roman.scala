package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _18_integer_to_roman extends AnyWordSpec with Matchers {
  def intToRoman(num: Int): String = {
    def value(v: Int, one: String, five: String, ten: String) =
      if (v < 4) one * v
      else if (v == 4) one + five
      else if (v == 5) five
      else if (v < 9) five + one * (v - 5)
      else one + ten

    val t = num / 1000
    val h = num % 1000 / 100
    val d = num % 100 / 10
    val u = num % 10

    ("M" * t) + value(h, "C", "D", "M") + value(d, "X", "L", "C") + value(u, "I", "V", "X")
  }

  "intToRoman" should {
    "work as expected" in {
      intToRoman(3749) shouldBe "MMMDCCXLIX"
      intToRoman(58) shouldBe "LVIII"
      intToRoman(1994) shouldBe "MCMXCIV"
    }
  }
}
