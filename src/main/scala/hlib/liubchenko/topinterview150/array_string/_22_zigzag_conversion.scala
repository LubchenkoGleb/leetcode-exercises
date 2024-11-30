package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _22_zigzag_conversion extends AnyWordSpec with Matchers {
  def convert(s: String, numRows: Int): String = ???

  "convert" should {
    "work as expected" in {
      convert("PAYPALISHIRING", 3) shouldBe "PAHNAPLSIIGYIR"
      convert("PAYPALISHIRING", 4) shouldBe "PINALSIGYAHRPI"
      convert("A", 1) shouldBe "A"
    }
  }
}
