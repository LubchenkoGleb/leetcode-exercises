package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _18_integer_to_roman extends AnyWordSpec with Matchers {
  def intToRoman(num: Int): String = ???

  "intToRoman" should {
    "work as expected" in {
      intToRoman(3749) shouldBe "MMMDCCXLIX"
      intToRoman(58) shouldBe "LVIII"
      intToRoman(1994) shouldBe "MCMXCIV"
    }
  }
}
