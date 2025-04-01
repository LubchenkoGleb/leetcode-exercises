package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _22_zigzag_conversion extends AnyWordSpec with Matchers {
  def convert(s: String, numRows: Int): String = if (numRows == 1) s
  else {
    def len(level: Int) = (level - 1) * 2

    val res = for {
      i <- 0 until numRows
      j <- i until s.length by len(numRows)
      r <- if (i == 0 || i == numRows - 1) List(j) else List(j, j + len(numRows - i))
      if r < s.length
    } yield s(r)

    res.mkString
  }

  "convert" should {
    "work as expected" in {
      convert("PAYPALISHIRING", 3) shouldBe "PAHNAPLSIIGYIR"
      convert("PAYPALISHIRING", 4) shouldBe "PINALSIGYAHRPI"
      convert("A", 1) shouldBe "A"
    }
  }
}
